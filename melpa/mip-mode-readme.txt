mip-mode is a minor mode which enables users to quickly jump between
virtual projects and their files.  MIP stands for Multiple Interactive
Projects.

mip-mode defines a workspace as a "root directory which contains
project subdirectories".

To enable mip globally, add the following to your .emacs file:

(global-mip-mode t)

Before mip-mode can be used, we must define a list of workspaces;
Workspaces are just directories which contain project subdirectories.

For example, I usually have a dedicated location for all of my
projects in my home directory:

~/workspace
~/workspace/project-1
~/workspace/project-2

To tell workspace directories to mip-mode, we must add the following
to our .emacs file:

(setq mip-workspaces '("~/workspace"))
or
(add-to-list 'mip-workspaces "~/workspace")

You can have multiple workspaces as long as their projects don't
conflict.

You can also ignore specific projects:

(setq mip-ignored-projects '("project-2"))
or
(add-to-list 'mip-ignored-projects "project-2")

You can give a specific project name or a regular expression to match
multiple projects.

You can tell mip to close all project specific buffers:

(setq mip-kill-project-buffers-on-close t)

mip checks each buffer if it's file resides in the project's
directory, and if it does, kills it.

mip can even kill the magit-status buffer:

(setq mip-kill-magit-status-buffer-on-close t)
