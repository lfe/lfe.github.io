"""
Note!! This code was borrowed from someone else. This script is a mangled
version of the otherone, necessitated by the fact that they didn't have comment
support implemented.

Some of the logic in this script is rather odd, and that's due to another fact:
the other script didn't properly sort the issues from the original repo, and as
such, the issues were created in reverse order in the new repo. Also, the
issues have different ids, since the closed issues weren't copied over too.
"""
from ConfigParser import SafeConfigParser
from StringIO import StringIO

import base64
import json
import urllib2


parser = SafeConfigParser()
parser.read("/etc/github")
username = parser.get("Personal Account", "username")
password = parser.get("Personal Account", "password")

server = "api.github.com"
src_repo = "lfe/lfe.github.com"
dst_repo = "lfe/lfe.github.io"
src_url = "https://%s/repos/%s" % (server, src_repo)
dst_url = "https://%s/repos/%s" % (server, dst_repo)


print "Using the following URLs:"
print "\tsource: %s" % src_url
print "\tdestination: %s" % dst_url
print 


def get_issues(url):
    req = urllib2.Request("%s/issues" % url)
    req.add_header("Authorization", "Basic " + base64.urlsafe_b64encode("%s:%s" % (username, password)))
    response = urllib2.urlopen(req)
    result = response.read()
    issues = json.load(StringIO(result))
    return issues


def get_comments(issue_id):
    url = "%s/issues/%s/comments" % (src_url, issue_id)
    req = urllib2.Request(url)
    req.add_header("Authorization", "Basic " + base64.urlsafe_b64encode("%s:%s" % (username, password)))
    response = urllib2.urlopen(req)
    result = response.read()
    comments = json.load(StringIO(result))
    return comments


def import_comments():
    src_issues = {}
    # build lookup table
    for src_issue in get_issues(src_url):
        src_issues[src_issue["title"]] = src_issue
    dst_issues = get_issues(dst_url)
    for issue in dst_issues:
        src_data = src_issues.get(issue["title"]) or {}
        src_id = src_data.get("number")
        comments = get_comments(src_id)
        if not comments:
            continue
        for comment in comments:
            url = "%s/issues/%s/comments" % (dst_url, issue["number"])
            data = json.dumps({"body": comment["body"]})
            headers = {
                "Authorization": "Basic " + base64.urlsafe_b64encode("%s:%s" % (username, password)),
                "Content-Type": "application/json",
                "Accept": "application/json"}
            req = urllib2.Request(url, data, headers)
            res = urllib2.urlopen(req)
            data = res.read()
            res_data = json.load(StringIO(data))
            print "Result: ", res_data


def error_check(func, args, err_msg):
    result = []
    try:
        result = func(*args)
    except Exception, err:
        print "%s Error when calling '%s': %s" % (
            err_msg, func.func_name, str(err))
    return result


def do_import():
    error_check(
        import_comments,
        [],
        "Couldn't import comments.")
