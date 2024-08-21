import sys
import re


def parse_github(s: str):
    """
    Parses GitHub URLs returns the owner/repo.
    """
    m = re.match("^(https://github.com/)?(.*)/(.*)$", s)
    if not m:
        raise ValueError("Unknown GitHub format: {}", s)
    match m.groups():
        case [_, owner, repo]:
            return f"{owner}/{repo}"
        case [owner, repo]:
            return f"{owner}/{repo}"
    
if __name__ == "__main__":
    print(parse_github(sys.argv[1]), file=sys.stdout)
