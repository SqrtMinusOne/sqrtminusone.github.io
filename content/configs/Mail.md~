+++
title = "Mail"
author = ["Pavel"]
draft = false
+++

My email configration. Currently I use [lieer](https://github.com/gauteh/lieer) to fetch emails from Gmail, [davmail](http://davmail.sourceforge.net/) & [offlineimap](http://www.offlineimap.org/) to fetch emails from MS Exchange, [notmuch](https://notmuchmail.org/) to index, [msmtp](https://marlam.de/msmtp/) to send emails. Also using notmuch frontend from Emacs.

My problem with any particular mail setup was that I use Gmail labels quite extensively, and handling these over IMAP is rather awkward. Notmuch seems to be the only software that provides the same first-class support for labels.

But I also have an Exchange account, with which I communicate via IMAP/SMTP adapter, and in this case, I synchronize notmuch tags and IMAP folders.

References:

-   [My post](https://sqrtminusone.xyz/posts/2021-02-27-gmail/) about email configuration. I wrote it some time ago, but the general idea remains.

<div class="ox-hugo-toc toc">
<div></div>

<div class="heading">Table of Contents</div>

- [Lieer](#lieer)
- [DavMail](#davmail)
- [OfflineIMAP](#offlineimap)
- [Notmuch](#notmuch)
    - [Config](#config)
    - [Hooks](#hooks)
        - [`pre_new`](#pre-new)
        - [`post_new`](#post-new)
- [Sync script](#sync-script)
- [MSMTP](#msmtp)
- [Emacs](#emacs)
    - [Saved filters and keybindings](#saved-filters-and-keybindings)
    - [Signing messages](#signing-messages)
- [mailcap](#mailcap)
- [Guix settings](#guix-settings)

</div>
<!--endtoc-->


## Lieer {#lieer}

| Guix dependency |
|-----------------|
| python-lieer    |

Lieer is a program to link up Gmail and notmuch. Basically, it downloads mail from Gmail via API, stores them in Maildir, and synchronizes labels with notmuch.

I have a separate directory in my `~/Mail` for each address. To init lieer, run the following command in the directory:

```text
gmi init <address>
```

After which the settings will be stored in `gmailieer.json` and the credentials in `.credentials.gmailieer.json`. The latter file is stored encrypted.

My preferred settings:

```text
gmi set --replace-slash-with-dot
gmi set --ignore-tags-local new
```

Running `gmi sync` in the required directory performs the synchronization. The first sync takes a while, the subsequent syncs are pretty fast.


## DavMail {#davmail}

is a gateway between MS Exchange and the rest of the world, which uses IMAP/SMTP/LDAP/etc. As I have one corporate MS Exchange address, this is just the program I need. As of yet, it isn't packaged for Guix, but it's easy enough to download.

It has a GUI mode, but I prefer headless config.

```ini
davmail.server=true
davmail.mode=Auto
davmail.url=https://mail.etu.ru/owa/

davmail.server.certificate.hash=0C:9E:CF:D3:62:26:DB:FA:F1:EE:36:9D:60:E7:31:71:CF:1F:92:85

davmail.caldavPort=1080
davmail.imapPort=1143
davmail.ldapPort=1389
davmail.popPort=1110
davmail.smtpPort=1025

davmail.imapAutoExpunge=false
davmail.enableKeepalive=false
```

Also it's a bit of problem to get it launched as it looks for its jars in the pwd, so here is a script.

```bash
cd $HOME/bin/davmail-6.0.0-3375
./davmail davmail.properties
```

Shepherd service is defined in [Desktop.org]({{< relref "Desktop" >}}).


## OfflineIMAP {#offlineimap}

| Guix dependency |
|-----------------|
| offlineimap     |

[OfflineIMAP](https://github.com/OfflineIMAP/offlineimap) is a program that can synchronize IMAP mailbox and Maildir. Lieer does everything by itself, but my pirate Exchange IMAP needs this program. There is also [isync](https://isync.sourceforge.io/), but there I had some weird issues with duplicate UIDs, which don't occur for OfflineIMAP.

I have a few options for setting a username and password. First, I can run `pass` in `remotepasswordeval`, and while this will work, it will keep my keyring unlocked because I want to run `offlineimap` every couple of minutes.

Another option is to use noweb and not push the file below to the version control. Then I have a plaintext password of email on my computer, but I think it's a lesser evil than the entire keyring.

I would use `password-store-get` from password-store.el, but I want this to be able to run without any 3rd party packages, so it's just bash.

<a id="code-snippet--mail-username"></a>
```bash
pass show Job/Infrastructure/pvkorytov@etu.ru | sed -n 's/username: //;2p'
```

<a id="code-snippet--mail-password"></a>
```bash
pass show Job/Infrastructure/pvkorytov@etu.ru | head -n 1
```

```ini
[general]
accounts = pvkorytov

[Account pvkorytov]
localrepository = pvkorytov-local
remoterepository = pvkorytov-remote

[Repository pvkorytov-local]
type = Maildir
localfolders = ~/Mail/pvkorytov_etu/

[Repository pvkorytov-remote]
type = IMAP
remotehost = localhost
remoteuser = <<mail-username()>>
remotepass = <<mail-password()>>
remoteport = 1143
starttls = no
ssl = no
```


## Notmuch {#notmuch}

| Guix dependency |
|-----------------|
| notmuch         |
| parallel        |

Notmuch is an email indexer program, which handles labels in a way somewhat similar to Gmail. It also provides a frontend for Emacs, but it's not the only one available.


### Config {#config}

Not much is going on here.

First, the database path.

```ini
[database]
path=/home/pavel/Mail
```

My name and list of emails. It's not like it's a secret anyhow.

```ini
[user]
name=Pavel Korytov
primary_email=thexcloud@gmail.com
other_email=progin6304@gmail.com;pvkorytov@etu.ru
```

A list of tags which will be added by `notmuch new` and directory names which will be ignored by `notmuch new`.

```ini
[new]
tags=new;
ignore=.osync_workdir;.mbsyncstate;.uidvalidity;.lock;/.*gmailieer\.json.*/
```

Exclude these tags from search by default.

```ini
[search]
exclude_tags=trash;spam;
```

Maildir compatibility.

```ini
[maildir]
synchronize_flags=true
```


### Hooks {#hooks}

Now we have to link up lieer & davmail's maildir and with notmuch. This is done via the notmuch hook system, which allows running custom scripts before and after any command.

With lieer and Gmail, it is enough to simply run the program, because Gmail has first-class support for tags. Maildir does not, so I decide to synchronize notmuch tags and IMAP folders. In essence, the idea is to:

-   move emails to their folders by tags _before_ the synchronization
-   tag mails by their folders _after_ the synchronization

The problem is that with that approach one email can have only one tag, but it's better than nothing.

So, here are the rules which match tags & folders:

<a id="table--pvkorytov-tags"></a>

| tag                      | folder                    |
|--------------------------|---------------------------|
| inbox                    | INBOX                     |
| sent                     | Sent                      |
| spam                     | Junk                      |
| trash                    | Trash                     |
| job.digital              | Job\_Digital              |
| job.digital.docs         | Job\_Digital.Docs         |
| job.digital.support      | Job\_Digital.Support      |
| job.digital.superservice | Job\_Digital.Superservice |

And below is a noweb function, which generates the following commands for notmuch to execute:

-   _before_ sync:
    -   `notmuch search --output files "NOT path:[PATH] AND tag:[TAG] AND tag:[ROOT_TAG]" | xargs -I ! mv ! [PATH]`
        Move emails with `TAG` but outside the matching `PATH` to the latter
    -   `notmuch search --output=files "NOT path:[ARCHIVE_PATH] AND tag:[ROOT_TAG] AND NOT tag:[TAG1] ... AND NOT tag:[TAGN]" | xargs -I ! mv ! [ARCHIVE_PATH]`
        Move untagged emails to the `ARCHIVE_PATH`
-   _after_ sync:
    -   `notmuch tag +[TAG] "path:[PATH] AND NOT tag:[TAG]"`
        Tag emails in `PATH` which do not yet have the matching `TAG`
    -   `notmuch tag -[TAG] "NOT path:[PATH] AND tag:[TAG] AND tag:[ROOT_TAG]"`
        Remove `TAG` from emails which are outside the matching `PATH`

These rules are getting included in the respective hooks.

<a id="code-snippet--mail-tags"></a>
```emacs-lisp
(setq my/maildir-root "~/Mail")

(let ((rules '()))
  (dolist (row tags)
    (let ((tag (nth 0 row))
	  (folder (nth 1 row)))
      (unless (string-empty-p make_tag)
	(add-to-list
	 'rules
	 (format "notmuch tag +%s \"path:%s/%s/cur/** AND NOT tag:%s\""
		 tag root folder tag)
	 t))
      (unless (string-empty-p remove)
	(add-to-list
	 'rules
	 (format "notmuch tag -%s \"NOT path:%s/%s/cur/** AND tag:%s AND tag:%s\""
		 tag root folder tag root_tag)
	 t))
      (unless (string-empty-p move)
	(add-to-list
	 'rules
	 (concat
	  (format "notmuch search --output=files \"NOT path:%s/%s/cur/** AND tag:%s AND tag:%s\""
		  root folder tag root_tag)
	  (format " | xargs -I ! mv ! %s/%s/%s/cur/" my/maildir-root root folder))
	 t))))
  (unless (string-empty-p archive_root)
    (add-to-list
     'rules
     (concat
      (format "notmuch search --output=files \"NOT path:%s/%s/cur/** AND %s AND tag:%s\""
	      root archive_root
	      (mapconcat
	       (lambda (row)
		 (format "NOT tag:%s" (car row)))
	       tags
	       " AND ")
	      root_tag)
      (format " | xargs -I ! mv ! %s/%s/%s/cur/" my/maildir-root root archive_root))
     t))
  (string-join rules "\n"))
```


#### `pre_new` {#pre-new}

This hook runs fetch from Gmail & offlineimap in parallel before the `notmuch new` command. The `parallel` command is provided by [GNU Parallel](https://www.gnu.org/software/parallel/).

It isn't necessary to run `cd` for offlineimap, but it's easier to write that way.

<a id="code-snippet--pre-new-pvkorytov-tags"></a>
```emacs-lisp
(my/mail-format-tags-rules tags "pvkorytov_etu" "pvkorytov" nil nil t "Archive")
```

```bash
# GMI="/home/pavel/Programs/miniconda3/envs/mail/bin/gmi"
GMI="gmi"

echo "Running pre-new filters"
<<mail-tags(move="t",archive_root="Archive")>>
echo "Pre-new filters done"

parallel --link -j0 "(cd /home/pavel/Mail/{1}/ && {2} {3})" ::: thexcloud progin6304 pvkorytov_etu ::: "$GMI" "$GMI" "offlineimap" ::: sync sync ""
```


#### `post_new` {#post-new}

And this hook tags different mailboxes with different tags.

<a id="code-snippet--post-new-pvkorytov-tags"></a>
```emacs-lisp
(my/mail-format-tags-rules tags "pvkorytov_etu" "pvkorytov" t t)
```

```bash
notmuch tag +main "path:thexcloud/** AND tag:new"
notmuch tag +progin "path:progin6304/** AND tag:new"
notmuch tag +pvkorytov "path:pvkorytov_etu/** AND tag:new"

echo "Running post-new filters"
<<mail-tags(make_tag="t",remove="t")>>
echo "Post-new filters done"
notmuch tag -new "tag:new"
```


## Sync script {#sync-script}

A script to run `notmuch new` and push a notification if there is new mail.

```bash
export DISPLAY=:0
CHECK_FILE="/home/pavel/Mail/.last_check"
QUERY="tag:unread"
ALL_QUERY="tag:unread"
if [ -f "$CHECK_FILE" ]; then
    DATE=$(cat "$CHECK_FILE")
    QUERY="$QUERY and date:@$DATE.."
fi

notmuch new
NEW_UNREAD=$(notmuch count "$QUERY")
ALL_UNREAD=$(notmuch count "$ALL_QUERY")

if [ $NEW_UNREAD -gt 0 ]; then
    MAIN_UNREAD=$(notmuch count "tag:unread AND tag:main")
    PROGIN_UNREAD=$(notmuch count "tag:unread AND tag:progin")
    ETU_UNREAD=$(notmuch count "tag:unread AND tag:pvkorytov")
    read -r -d '' NOTIFICATION <<EOM
$NEW_UNREAD new messages
$MAIN_UNREAD thexcloud@gmail.com
$PROGIN_UNREAD progin6304@gmail.com
$ETU_UNREAD pvkorytov@etu.ru
$ALL_UNREAD total
EOM
    notify-send "New Mail" "$NOTIFICATION"
fi

echo "$(date +%s)" > $CHECK_FILE
```

The script is ran via GNU Mcron every 5 minutes.

```scheme
(job "*/5 * * * * " "~/bin/scripts/check-email")
```


## MSMTP {#msmtp}

| Guix dependency |
|-----------------|
| msmtp           |

Sending emails can be done with MSMTP. It automatially chooses the email address and server based on the contents of the message, which is handy if there are multiple mailboxes to be managed.

```vim
defaults
auth on
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt
logfile ~/.msmtp.log

account main
host smtp.gmail.com
port 587
from thexcloud@gmail.com
user thexcloud@gmail.com
passwordeval "pass show My_Online/APIs/google-main-app-password | head -n 1"

account progin
host smtp.gmail.com
port 587
from progin6304@gmail.com
user progin6304@gmail.com
passwordeval "pass show My_Online/ETU/progin6304@gmail.com | head -n 1"

account pvkorytov
tls off
auth plain
host localhost
port 1025
from pvkorytov@etu.ru
user pvkorytov
passwordeval "pass show Job/Infrastructure/pvkorytov@etu.ru | head -n 1"
```


## Emacs {#emacs}

Finally, Emacs configuration. Let's start with some variables:

```emacs-lisp
(setq user-mail-address "thexcloud@gmail.com")
(setq user-full-name "Pavel Korytov")
```

Then, the problem with my Guix setup is that Emacs by default doesn't see the elisp files of notmuch, so here is a small workaround:

```emacs-lisp
(let ((default-directory  "/home/pavel/.guix-extra-profiles/mail/mail/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

```

Finally the proper notmuch settings:

```emacs-lisp
(use-package notmuch
  ;; :ensure nil
  :commands (notmuch notmuch-search)
  :config
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq notmuch-always-prompt-for-sender t)
  (setq sendmail-program (executable-find "msmtp"))
  (setq send-mail-function #'sendmail-send-it)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
  (add-hook 'notmuch-hello-mode-hook
	    (lambda () (display-line-numbers-mode 0))))
```

The file to which this is tangled is read in the init.el.


### Saved filters and keybindings {#saved-filters-and-keybindings}

I want to have the saved filters available in both notmuch interface as as keybindings. So a bit more of abusing org tables.

Root keybindings:

```emacs-lisp
(my-leader-def
  :infix "am"
  "" '(:which-key "notmuch")
  "m" 'notmuch)
```

<a id="table--root-tags"></a>

| Root tag  | Prefix | Keybinding description |
|-----------|--------|------------------------|
| main      | t      | thexcloud@gmail.com    |
| progin    | p      | progin6304@gmail.com   |
| pvkorytov | e      | pvkorytov@etu.ru       |

<a id="table--filter-tags"></a>

| Tag    | Prefix | Name     |
|--------|--------|----------|
| inbox  | i      | inbox    |
| unread | u      | unread   |
| sent   | s      | sent     |
|        | a      | all mail |

The following formats the tables above to a proper syntax for `setq notmuch-saved-searches`:

<a id="code-snippet--format-notmuch-saved-searches"></a>
```emacs-lisp
(let ((searches '()))
  (dolist (root_tag root_tags)
    (dolist (tag filter_tags)
      (add-to-list
       'searches
       (format "(:name \"%s\" :query \"%s\")"
	       (format "%s (%s)"
		       (nth 0 root_tag)
		       (nth 2 tag))
	       (concat "tag:" (nth 0 root_tag)
		       (unless (string-empty-p (nth 0 tag))
			 (concat " AND tag:" (nth 0 tag)))))
       t)))
  (string-join searches "\n"))
```

And the following does the same for my general.el definer:

<a id="code-snippet--format-notmuch-keybindings"></a>
```emacs-lisp
(let ((bindings '()))
  (dolist (root_tag root_tags)
    (add-to-list
     'bindings
     (format "\"%s\" '(:which-key \"%s\")"
	     (nth 1 root_tag)
	     (nth 2 root_tag))
     t)
    (dolist (tag filter_tags)
      (add-to-list
       'bindings
       (format "\"%s\" '((lambda () (interactive) (notmuch-search \"%s\")) :which-key \"%s\")"
	       (concat (nth 1 root_tag) (nth 1 tag))
	       (concat "tag:" (nth 0 root_tag)
		       (unless (string-empty-p (nth 0 tag))
			 (concat " AND tag:" (nth 0 tag))))
	       (nth 2 tag))
       t)))
  (string-join bindings "\n"))
```

```emacs-lisp
(setq notmuch-saved-searches
      '((:name "drafts" :query "tag:draft")
	<<format-notmuch-saved-searches()>>))

(my-leader-def
  :infix "am"
  <<format-notmuch-keybindings()>>)
```


### Signing messages {#signing-messages}

```emacs-lisp
(with-eval-after-load 'notmuch
  (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime))

(setq mml-secure-key-preferences
      '((OpenPGP
	 (sign
	  ("thexcloud@gmail.com" "914472A1FD6775C166F96EBEED739ADF81C78160"))
	 (encrypt))
	(CMS
	 (sign)
	 (encrypt))))
```


## mailcap {#mailcap}

mailcap file is a file which defines how to read to different MIME types. Notmuch also uses it, so why not keep it here.

```text
audio/*; mpc add %s

image/*; feh %s

application/msword; /usr/bin/xdg-open %s
application/pdf; zathura %s
application/postscript ; zathura %s

text/html; firefox %s
```


## Guix settings {#guix-settings}

<a id="code-snippet--packages"></a>
```emacs-lisp
(my/format-guix-dependencies)
```

```scheme
(specifications->manifest
 '(
   <<packages()>>))
```