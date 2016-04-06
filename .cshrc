# /etc/cshrc
#
#  System wide Cshrc
#
#  If you want to modify the Cshrc file is organized
#  then copy the Cshrc file to your home directory and "source"
#  that one.
#

if ( -f ~/Common.cshrc ) then
    source ~/Common.cshrc
endif

#
#  System wide comet aliases
#
#
#  If you want to modify any of how the Cshrc file is organized
#  then copy the Cshrc file to your home directory and "source"
#  that one.
#
#  If you have a file in your home directory called ".aliases", then
#  it will be sourced here.
#
if ( -f Common.aliases ) then
    source ~/Common.aliases
endif

if ( -f .login ) then
   source ~/.login
endif
