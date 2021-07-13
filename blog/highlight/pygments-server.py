import json
import sys
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

formatter = HtmlFormatter(nowrap=True)

for request_line in sys.stdin:
    request = json.loads(request_line)
    lexer = get_lexer_by_name(request["language"], encoding="utf-8")
    response = highlight(request["source"], lexer, formatter).rstrip()
    json.dump(response, sys.stdout, ensure_ascii=False)
    sys.stdout.flush()
