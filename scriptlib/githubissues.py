"""
NOTE!! This script was copied from someone else, and it doesn't insert issues
in the correct order. If you want to use this, try reversing the list of issues
before adding them to your new repo.
"""
from ConfigParser import SafeConfigParser
from StringIO import StringIO

import base64
import json
import urllib2


#==== configurations =======
src_repo = "lfe/lfe.github.com"
dst_repo = "lfe/lfe.github.io"
#==== end of configurations ===

parser = SafeConfigParser()
parser.read("/etc/github")
parser.get("Personal Account", "username")
parser.get("Personal Account", "password")

server = "api.github.com"
src_url = "https://%s/repos/%s" % (server, src_repo)
dst_url = "https://%s/repos/%s" % (server, dst_repo)

print "Using the following URLs:"
print "\tsource: %s" % src_url
print "\tdestination: %s" % dst_url
print

def get_labels(url):
    response = urllib2.urlopen("%s/labels" % url)
    result = response.read()
    labels = json.load(StringIO(result))
    return labels

def get_issues(url):
    response = urllib2.urlopen("%s/issues" % url)
    result = response.read()
    issues = json.load(StringIO(result))
    return issues

def get_comments_on_issue(issue):
    if issue.has_key("comments") \
      and issue["comments"] is not None \
      and issue["comments"] != 0:
        response = urllib2.urlopen("%s/comments" % issue["url"])
        result = response.read()
        comments = json.load(StringIO(result))
        return comments
    else :
        return []

def import_milestones(milestones):
    for source in milestones:
        dest = json.dumps({
            "title": source["title"],
            "state": "open",
            "description": source["description"],
            "due_on": source["due_on"]})

        req = urllib2.Request("%s/milestones" % dst_url, dest)
        req.add_header("Authorization", "Basic " + base64.urlsafe_b64encode("%s:%s" % (username, password)))
        req.add_header("Content-Type", "application/json")
        req.add_header("Accept", "application/json")
        res = urllib2.urlopen(req)

        data = res.read()
        res_milestone = json.load(StringIO(data))
        print "Successfully created milestone %s" % res_milestone["title"]

def import_labels(labels):
    for source in labels:
        dest = json.dumps({
            "name": source["name"],
            "color": source["color"]
        })

        req = urllib2.Request("%s/labels" % dst_url, dest)
        req.add_header("Authorization", "Basic " + base64.urlsafe_b64encode("%s:%s" % (username, password)))
        req.add_header("Content-Type", "application/json")
        req.add_header("Accept", "application/json")
        res = urllib2.urlopen(req)

        data = res.read()
        res_label = json.load(StringIO(data))
        print "Successfully created label %s" % res_label["name"]

def import_issues(issues, dst_milestones, dst_labels):
    for source in issues:
        labels = []
        if source.has_key("labels"):
            for src_label in source["labels"]:
                name = src_label["name"]
                for dst_label in dst_labels:
                    if dst_label["name"] == name:
                        labels.append(name)
                        break

        milestone = None
        if source.has_key("milestone") and source["milestone"] is not None:
            title = source["milestone"]["title"]
            for dst_milestone in dst_milestones:
                if dst_milestone["title"] == title:
                    milestone = dst_milestone["number"]
                    break

        assignee = None
        if source.has_key("assignee") and source["assignee"] is not None:
            assignee = source["assignee"]["login"]

        body = None
        if source.has_key("body") and source["body"] is not None:
            body = source["body"]

        dest = json.dumps({
            "title": source["title"],
            "body": body,
            "assignee": assignee,
            "milestone": milestone,
            "labels": labels
        })

        comments = get_comments_on_issue(source)
        #todo: insert logic on comments if needed

        req = urllib2.Request("%s/issues" % dst_url, dest)
        req.add_header("Authorization", "Basic " + base64.urlsafe_b64encode("%s:%s" % (username, password)))
        req.add_header("Content-Type", "application/json")
        req.add_header("Accept", "application/json")
        res = urllib2.urlopen(req)

        data = res.read()
        res_issue = json.load(StringIO(data))
        print "Successfully created issue %s" % res_issue["title"]


def error_check(func, args, err_msg):
    result = []
    try:
        result = func(*args)
    except Exception, err:
        print "%s Error when calling '%s': %s" % (
            err_msg, func.func_name, str(err))
    return result


def do_import():
    #get milestones and issues to import
    milestones = error_check(
        get_milestones, [src_url], "Couldn't get source milestones.")
    labels = error_check(
        get_labels, [src_url], "Couldn't get source labels.")
    #do import
    error_check(
        import_milestones, [milestones], "Couldn't import milestones.")
    error_check(
        import_labels, [labels], "Couldn't import labels.")
    #get imported milestones and labels
    milestones = error_check(
        get_milestones, [dst_url], "Couldn't get imported milestones.")
    labels = error_check(
        get_labels, [dst_url], "Couldn't get imported labels.")

    #process issues
    issues = error_check(get_issues, [src_url], "Couldn't get source issues.")
    error_check(
        import_issues, [issues, milestones, labels],
        "Couldn't import issues.")