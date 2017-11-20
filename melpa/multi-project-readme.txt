Multi-project simplifies the switching between different projects by
providing support for creating, deleting, and searching between projects.
Multi-project supports interactively finding a file within a project by
automatically switching the TAGS file for symbol lookup.

To use multi-project add the following lines within your .emacs file:

(require 'multi-project)
(global-multi-project-mode)

The following bindings are created for multi-project
C-xpj - Project jump              Displays a list of projects
C-xpc - Project compile           Run the compilation command for a project
C-xpa - Anchor a project          Stores the project to be retrieved via
                                  multi-project-anchored
C-xpg - Run grep-find             Runs grep-find at project top
C-xpu - Resets the anchor         Clears out the multi-project-anchored var
C-xpl - Last project from anchor  Jumps to the project stored via the anchor
C-xpp - Present project           Displays current project
C-xpv - Vist current project      Visits current project
C-xpf - Find project files        Interactively find project files
C-xpn - Add a new project         Prompts for new project information
C-xpr - Go to project root        Displays the project root

When displaying the projects, the following bindings are present:
s     - Search projects:          Searches from the list of possible projects
C-n   - Next project              Move the cursor to the next project
C-p   - Previous project          Move the cursor to the previous project
a     - Anchor a project          Holds the last project to the anchored
                                  project
r     - Reset search              Resets the project search
N     - Add new project           Prompts for project information
d     - Delete a project          Marks the project for deletion
g     - Grep a project            Executes grep-find in the selected projects
u     - Unmark a project          Removes the mark for a project
x     - Executes actions          Executes the deletions
q     - quit

The multi-project-compilation-command variable can be set to a function
that provides a customized compilation command.  For example,

(defun my-compilation-command (project-list)
  (let ((project-name (car project-list))
	   (project-dir (nth 1 project-list))
	   (project-subdir (nth 2 project-list)))

    (cond ((string-match "proj1" project-name)
	      (concat "ant -f " project-dir "/" project-subdir "/build.xml"))
	     (t
	      (concat "make -C " project-dir "/" project-subdir)))))

(setq multi-project-compilation-command 'my-compilation-command)
