import bs4
import gzip
from urllib.request import urlopen

DOCS_URL = "http://research.microsoft.com/en-us/projects/dafny/reference.aspx"

def cleanup_code_spans(node):
    count = 0
    for span in node.find_all("span", style="COLOR: #0000ff"):
        span.attrs.clear()
        span.name = 'code'
        if isinstance(span.next_sibling, bs4.element.NavigableString):
            if span.next_sibling.startswith("<T>"):
                span.next_sibling.string.replace_with(span.next_sibling.string[len("<T>"):])
                span.string += "<T>"
        count += 1
    return count

def cleanup(node):
    cleanup_code_spans(node)
    for p in node.find_all("p"):
        if next(iter(p.children)).name == "code":
            p.name = "pre"
            for x in p(lambda x: x.name not in ["code", "br"]):
                x.unwrap()

def find_content_node(soup):
    return soup.find(**{"class": "conM"})

def read_clean_docs():
    with urlopen(DOCS_URL) as page:
        soup = bs4.BeautifulSoup(page.read())
        top = find_content_node(soup)
        cleanup(top)
    return top

with gzip.open("dafny-docs.html.gz", mode='wb') as writer:
    writer.write(str(read_clean_docs()).encode("UTF-8"))
