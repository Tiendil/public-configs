
#------------------------------------------------------------------------------
# NotebookApp(JupyterApp) configuration
#------------------------------------------------------------------------------

## Reload the webapp when changes are made to any Python src files.
#  Default: False
# c.NotebookApp.autoreload = False

## Whether to open in a browser after starting. The specific browser used is
#  platform dependent and determined by the python standard library `webbrowser`
#  module, unless it is overridden using the --browser (NotebookApp.browser)
#  configuration option.
#  Default: True
c.NotebookApp.open_browser = False

## DISABLED: use %pylab or %matplotlib in the notebook to enable matplotlib.
#  Default: 'disabled'
# c.NotebookApp.pylab = 'disabled'

## If True, display a button in the dashboard to quit (shutdown the notebook
#  server).
#  Default: True
c.NotebookApp.quit_button = False

## Supply overrides for terminado. Currently only supports "shell_command". On
#  Unix, if "shell_command" is not provided, a non-login shell is launched by
#  default when the notebook server is connected to a terminal, a login shell
#  otherwise.
#  Default: {}
# c.NotebookApp.terminado_settings = {}

## Set to False to disable terminals.
#
#  This does *not* make the notebook server more secure by itself. Anything the
#  user can in a terminal, they can also do in a notebook.
#
#  Terminals may also be automatically disabled if the terminado package is not
#  available.
#  Default: True
# c.NotebookApp.terminals_enabled = True

## Disable launching browser by redirect file
#
#  For versions of notebook > 5.7.2, a security feature measure was added that
#  prevented the authentication token used to launch the browser from being
#  visible. This feature makes it difficult for other users on a multi-user
#  system from running code in your Jupyter session as you.
#
#  However, some environments (like Windows Subsystem for Linux (WSL) and
#  Chromebooks), launching a browser using a redirect file can lead the browser
#  failing to load. This is because of the difference in file structures/paths
#  between the runtime and the browser.
#
#  Disabling this setting to False will disable this behavior, allowing the
#  browser to launch by using a URL and visible token (as before).
#  Default: True
# c.NotebookApp.use_redirect_file = True

## Specify Where to open the notebook on startup. This is the `new` argument
#  passed to the standard library method `webbrowser.open`. The behaviour is not
#  guaranteed, but depends on browser support. Valid values are:
#
#   - 2 opens a new tab,
#   - 1 opens a new window,
#   - 0 opens in an existing window.
#
#  See the `webbrowser.open` documentation for details.
#  Default: 2
# c.NotebookApp.webbrowser_open_new = 2

#------------------------------------------------------------------------------
# FileContentsManager(FileManagerMixin, ContentsManager) configuration
#------------------------------------------------------------------------------

## If True (default), deleting files will send them to the platform's
#  trash/recycle bin, where they can be recovered. If False, deleting files
#  really deletes them.
#  Default: True
c.FileContentsManager.delete_to_trash = False
