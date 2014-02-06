# fromrc.pl --- configuration file for `from' script
# Author: Noah Friedman <friedman@splode.com>
# Created: 1995-10-22
# Public domain

# $Id: fromrc.pl,v 1.9 2008/11/10 06:44:00 friedman Exp $

$opt_numbered   = 1;
$opt_firstlinep = 1;

# Default output with line numbers.
# This is equivalent to not specifying an output format and using -n
#
#$opt_format = "%-3{FLINES} %-19.18From %-13.12Date %Subject\n";

# Fancy output format.  I recommend enabling $opt_no_parsep if you use this
# format.
#
#$opt_format     = "From: %From\nTo: %To\nCc: %Cc\nSubject: %Subject\nDate: %Date\n\n";
#$opt_no_parsep  = 1;
#$opt_firstlinep = 0;

# Other options you can set.

#$opt_columns = 79;
if (($ENV{TERM} eq 'emacs' || exists $ENV{EMACS})
    || (defined $ENV{LC_CTYPE} && $ENV{LC_CTYPE} =~ /utf-?8/i))
  {
    $opt_columns  = 0;
    $opt_utf8 = 1;
  }

#$opt_mail = $ENV{MAIL} || "/var/spool/mail/$ENV{USER}";
#$opt_mail = "pop:HOST:PORT:pass:$ENV{USER}:*";
#$opt_mail = "imap:HOST:PORT:MAILBOX:login:$ENV{USER}:*";
#$opt_pass = $ENV{POPPASS} || 'fnord';

# See if a proxy redirect is running
#system ('netstat -an | egrep "(127|0)\.0\.0\.[01]:1100 .*LIST" > /dev/null');
#$opt_mail = "pop:localhost:1100:pass:$ENV{USER}:*" if ($? == 0);

# fromrc.pl ends here
