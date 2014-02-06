# readlink.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1993-02-07
# Last modified: 1993-02-07
# Public domain

# Commentary:

# These routines require perl.

# Code:

#:docstring readlink:
# Usage: readlink [path]
#
# If PATH is a symlink, write on stdout the path that it points to.  An
# error message is echoed to stderr and readlink returns a nonzero exit
# status if PATH is not a symlink.
#:end docstring:

###;;;autoload
function readlink ()
{
    perl - "$@" <<- '__EOF__'
        if ($l = readlink ("$ARGV[0]"))
          {
           print "$l\n";
          }
        else
          {
           print STDERR "readlink: $ARGV[0]: $!\n";
           exit (1);
          }
	__EOF__
}

#:docstring symlink_resolve:
# Usage: symlink_resolve [path1] {path2} {...}
#
# Resolve pathnames until there are no more symbolic links.  Resulting
# pathnames be not be files that actually exists (depending on whether
# symlinks point to nonexistent files).
#
# If the environment variable SYMLINK_RESOLVE is set to "canonicalize",
# then pathnames are cleaned up of internal relative pathnames, i.e.
# canonicalization to absolute pathname is done.
#
# In order to avoid infinite loops, this function will give up on a
# particular pathname if there are more than 16 levels of symbolic links.
# Perhaps a complicated algorithm could "prove" whether a loop is really
# present, but it's probably too expensive.  In any case, this function
# allows twice as many levels of symlinks as most kernels do when they do
# internal symlink resolution (they usually only allow 8).
#:end docstring:

###;;;autoload
function symlink_resolve ()
{
    pwd=$(pwd) perl - "$@" <<- '__EOF__'
        $pwd="$ENV{'pwd'}";
        next_path:
        for ($i = 0; $i < @ARGV; $i++)
          {
           @p = split (/\//, "$ARGV[$i]");
           $link_count = 0;
           for ($j = 0; $j < @p; $j++)
             {
              $orig_component = $k = join ("/", @p[0 .. $j]);
              while ($l = readlink($k))
                {
                 $k = $l;
                 # Simple way of detecting symlink loops (it unfortunately
                 # causes the system to give up when there are simply too
                 # many levels, even if resolution would eventually occur).
                 # This parameter is adjustable, of course.  Most unix
                 # kernels allow a depth of 8.
                 if ($link_count++ == 16)
                   {
                    print STDERR "symlink_resolve: $ARGV[$i]: " . 
                                 "Too many levels of symbolic links\n";
                    next next_path;
                   }
                }
              if ($k eq $orig_component)
                {
                 next;
                }
              if (substr ($k, 0, 1) eq "/")
                {
                 # Absolute link.  Trash p[0]-p[j+1] and replace with
                 # readlinked path components.  Set j to -1 so that next
                 # iteration of loop will check array @p from start.
                 splice (@p, 0, $j + 1, split (/\//, $k));
                 $j = -1; 
                }
              else
                {
                 # Insert partial (relative) path component into array in
                 # place of current element p[j]
                 splice (@p, $j, 1, split (/\//, $k));
                 $j--;
                }
             }
           $_ = join ("/", @p);

           # If pathname should not be canonicalized, exit here. 
           if ($ENV{'SYMLINK_RESOLVE'} ne 'canonicalize')
             {
              print "$_\n";
              exit (0);
             }

           # Canonicalize pathname. 
           s/^\.$/$pwd/o;                    # Replace single "." with pwd.
           s/^([^\/])/$pwd\/\1/o;            # Prepend pwd if relative.
           s/\/\.\//\//og;                   # Remove any occurence of "/./".
           s/^.*\/\//\//og;                  # Get rid of "//" occurences.
           # Must do this in a loop to handle overlapping "/" character in
           # instances of "/../../"
           while (/\/[^\/][^\/]*\/\.\.\//o)  
             {
              s/\/[^\/][^\/]*\/\.\.\//\//og; # Resolve most references to ".."
             }
           s/\/[^\/][^\/]*\/\.\.$//o;        # Resolve trailing ".."
           s/^\/..\//\//go;                  # Eliminate leading "/.."
           print "$_\n";   
          }
	__EOF__
}

#:docstring symlink_resolve_canonicalize:
# Usage: symlink_resolve_canonicalize [path1] {path2} {...}
#
# Like symlink_resolve, but force canonicalization of resolved pathnames. 
#:end docstring:

###;;;autoload
function symlink_resolve_canonicalize ()
{
    SYMLINK_RESOLVE=canonicalize symlink_resolve "$@"
}

provide readlink

# readlink.bash ends here
