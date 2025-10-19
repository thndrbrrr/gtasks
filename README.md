# gtasks.el

`gtasks.el` is a synchronous Emacs Lisp client for the Google Tasks REST API. It provides functions for listing, creating, and modifying tasklists and tasks.

## Installation

Add the repository to your `load-path` and require the library:

```elisp
(add-to-list 'load-path "/path/to/gtasks")
(require 'gtasks)
```

If you use `use-package`, you can load it lazily:

```elisp
(use-package gtasks
  :load-path "~/src/gtasks")
```

## Authorization

1. Visit the [Google Cloud Console](https://console.cloud.google.com/apis/credentials) and under `APIs & Services > Credentials` create **OAuth client credentials** of type **Desktop**.
2. Copy the generated **Client ID** and **Client Secret** into the relevant customization options (see below).
3. Go to `APIs & Services > Library`, search for `Google Tasks API`, click on the result, and click `Enable`.
4. Setup credentials in Emacs (see *Securely providing credentials* below).
5. Run `(gtasks-authorize)` once, authorize your app in the browser, and paste the authorization code back into Emacs when prompted.
6. Tokens are cached in `~/.emacs.d/.gtasks/token.json` by default and refreshed automatically when they expire.

> **Tip:** Ensure the token directory is only readable by you. `gtasks.el` creates it with `0700` permissions.

### Securely providing credentials

You can configure `gtasks.el` using either a plain client secret (which is not recommended) or, preferably, by defining a function that returns the client secret. Below is an example of how to use [authinfo](https://www.gnu.org/software/emacs/manual/html_node/emacs/Authentication.html) to securely retrieve the client secret:

``` elisp
(setq gtasks-client-id
      "some-client-id.apps.googleusercontent.com")

(setq gtasks-client-secret
	(lambda ()
		(auth-source-pick-first-password :host "google-api-creds")))
```

## Usage

The library exposes synchronous helpers that return plists matching the Google Tasks responses.

List tasklists:

```elisp
(let ((lists (gtasks-tasklist-list)))
  (mapcar (lambda (list) (plist-get list :title))
          (plist-get lists :items)))
```

Create a task:

```elisp
(let* ((list-id (gtasks-tasklist-id-by-title "Personal"))
       (task (gtasks-task-insert list-id '(:title "Review pull requests"))))
  (plist-get task :id))
```

Mark a task as complete:

```elisp
(gtasks-task-complete list-id task-id)
```

List tasks with pagination handled automatically:

```elisp
(gtasks-task-list list-id)
```

For more entry points, inspect the functions beginning with `gtasks-tasklist` and `gtasks-task`.

## Limitations

Currently not supporting nested tasks.

## Customization

All package options live under `M-x customize-group RET gtasks RET`.

- **`gtasks-client-id`** – OAuth2 client ID (string).
- **`gtasks-client-secret`** – Client secret, either a string or a zero-argument function returning the secret.
- **`gtasks-token-directory`** – Directory where tokens are cached (`~/.emacs.d/.gtasks/` by default).
- **`gtasks-token-file`** – File inside the token directory storing the refresh token (`token.json` by default).
- **`gtasks-user-agent`** – HTTP User-Agent header value sent with API requests.
- **`gtasks-timeout`** – Timeout in seconds for HTTP calls.
- **`gtasks-api-root`** – Base URL for the Tasks API (override for testing or proxies).

Adjust these variables either through Customize or by setting them in your init file prior to requiring `gtasks.el`.

## License

Distributed under the terms of the GNU General Public License Version 3.  See `LICENSE` for details.
 
