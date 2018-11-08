#!/bin/bash

# Source me from the containing directory.

export DETMONAD_DIR=`pwd`

# Option 0: weak version, just bring with_libdet & friends into scope.
# --------------------------------------------------------------------

# export PATH=$PATH:$DETMONAD_DIR/bin


# Option 1: stay in the current shell, bring detflow in path
# -----------------------------------------------------------

# # Make sure everything is built:
# stack build

# # Then we can ask WHERE it is:
# DETFLOW=$(stack exec -- which detflow)
# RUNGHC=$(stack exec -- which runghc)

# # This is weaker than doing `stack exec bash`, but it gets the
# # detflow/detmake binary into scope.
# export PATH=$PATH:$DETMONAD_DIR/bin:`dirname $DETFLOW`:`dirname $RUNGHC`
# # Compared to `stack exec`, this doesn't set:
# #   GHC_PACKAGE_PATH HASKELL_DIST_DIR HASKELL_PACKAGE_SANDBOXES HASKELL_PACKAGE_SANDBOX STACK_EXEC


# Option 2: run a nested shell session.
# -------------------------------------------

# Just let stack do the work for us.  Plus, bring with_libdet into scope:
stack build
PATH=$PATH:$DETMONAD_DIR/bin stack exec -- bash -i 
