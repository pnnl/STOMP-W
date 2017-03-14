#!/usr/bin/perl
# $Id: surfaceTo.pl 1080 2017-03-14 16:22:02Z d3c002 $
#
#  Print banner.
#
print("\nWelcome to surfaceTo ...\n\n");
print("This perl program transforms a STOMP surface file into\n");
print("formatted input file for Gnuplot, Grapher, Igor, Matlab, Plotmtv, SciDAVis, and Tecplot.\n");
print("Entries not made on the command line will be prompted.\n\n");
#
#  Initialize variables.
#
$nargv = $#ARGV;
$t_opt = 0;
$ts_opt = 0;
$av_opt = 0;
$logx_opt = 0;
$logy_opt = 0;
#
#  Check command line for options.
#
while ( $ARGV[0] =~ /^-/i ) {
  if( $ARGV[0] =~ /help\b/i ) {
    print "Command Line Entry:\n";
    print "    [Options]\n";
    print "    Plotting Package Option [Gnuplot|Grapher|Igor|MatLab|Plotmtv|SciDAVis|Tecplot]\n";
    print "    Plotting Package File to Generate\n";
    print "    STOMP Surface File to Convert\n\n";
    print "Options:\n\n";
    print "-help      this help\n";
    print "-a        all variables\n\n";
    print "-t         title prompt option (Tecplot and Gnuplot only)\n";
    print "-logx      plot time on logarithmic scale (Gnuplot only)\n";
    print "-logy      plot variables on logarithmic scale (Gnuplot only)\n\n";
    print "Plotting Package Options:\n\n";
    print "Gnuplot\n";
    print "Grapher\n";
    print "Igor\n\n";
    print "MatLab\n\n";
    print "Plotmtv\n";
    print "SciDAVis\n";
    print "Tecplot\n";
    print "Examples:\n\n";
    print "surfaceTo.pl -t Gnuplot surface.dat surface \n";
    print "  -- prints (variables and title will be prompted) to file output.dat\n";
    print "surfaceTo.pl -a Tecplot surface.dat surface \n";
    print "  -- prints all variables to file surface.dat\n";
    print "surfaceTo.pl -a plotmtv mass_flux.dat surface \n";
    print "  -- prints all variables to file mass_flux.dat\n";
    print "surfaceTo.pl plotmtv heat_flux.dat surface \n";
    print "  -- prints (variables will be prompted) to file heat_flux.dat\n";
    print "surfaceTo.pl Grapher surface.dat surface\n";
    print "  -- prints (variables and surfaces will be prompted) to file surface.dat\n";
    print "surfaceTo.pl -av Tecplot surface.dat surface\n";
    die   "  -- prints all variables (surfaces will be prompted) to file surface.dat\n\n";
#
#  All variables option
#
  } elsif( $ARGV[0] =~ /\-av\b/ || $ARGV[0] =~ /\-a\b/ ) {
    $av_opt = 1;
    shift(@ARGV);
#
#  Tecplot or Gnuplot title option
#
  }elsif( $ARGV[0] =~ /\-t\b/i ) {
   $t_opt = 1;
   shift(@ARGV);
#
#  Gnuplot logx option
#
  }elsif( $ARGV[0] =~ /\-logx\b/i ) {
   $logx_opt = 1;
   shift(@ARGV);
#
#  Gnuplot logy option
#
  }elsif( $ARGV[0] =~ /\-logy\b/i ) {
   $logy_opt = 1;
   shift(@ARGV);
#
#  Unrecognized option
#
  } else {
    die "Error: Unrecognized Option: $ARGV[0]\n";
  }
}
#
#  Search for plotting package name as first argument or prompt user.
#
if( $ARGV[0] ) {
  $plot_package = $ARGV[0];
  chomp( $plot_package );
  if( $plot_package =~ /^tecplot\b/i ) {
    print("Plotting Package: Tecplot\n");
  } elsif( $plot_package =~ /^grapher\b/i ) {
    print("Plotting Package: Grapher\n");
  } elsif( $plot_package =~ /^gnuplot\b/i ) {
    print("Plotting Package: Gnuplot\n");
  } elsif( $plot_package =~ /^igor\b/i ) {
    print("Plotting Package: Igor\n");
  } elsif( $plot_package =~ /^matlab\b/i ) {
    print("Plotting Package: MatLab\n");
  } elsif( $plot_package =~ /^plotmtv\b/i ) {
    print("Plotting Package: Plotmtv\n");
  } elsif( $plot_package =~ /^scidavis\b/i ) {
    print("Plotting Package: SciDAVis\n");
  } else {
    die "Error: Unrecognized Plotting Package: $plot_package.\n\n";
  }
#
#  Search for plotting-package input file name as second argument or prompt user.
#
  if( $ARGV[1] ) {
    $out_file = $ARGV[1];
    chomp( $out_file );
    open( OUT,">$out_file") || die "Error: Unable to Open Plotting-Package File Name: $out_file.\n";
#
#  Search for STOMP surface file as third argument or prompt user.
#
    if( $ARGV[2] ) {
#
#  Check for multiple plotting-package input files
#
      if( $#ARGV > 2 ) {
        die "Error: Multiple STOMP Surface Files Specified.\n";
      }
      $in_file = $ARGV[2];
#
#  No third(+) argument; ask user for STOMP surface file name.
#
    } else {
      $stops = 1;
      do {
        print("STOMP Surface File Name?\n");
        $in_file = <STDIN>;
        chomp( $in_file );
        if( -r $in_file ) {
          $stops = 1;
        } elsif( -B $in_file )  {
          $stops = 0;
          print("Error: STOMP Surface File is Binary: $in_file[$nf].\n");
        } else {
          $stops = 0;
          print("Error: STOMP Surface File is Unreadable: $in_file[$nf].\n");
        }
        if( $stops == 0 ) { print("Try again!\n\n"); }
      } until $stops != 0;
    }
#
#  No second argument; ask user for plotting-package input file name.
#
  } else {
    $stops = 0;
    do {
      if( $plot_package =~ /^tecplot\b/i ) {
        print("Tecplot File to Generate?\n");
      } elsif( $plot_package =~ /^grapher\b/i ) {
        print("Grapher File to Generate?\n");
      } elsif( $plot_package =~ /^gnuplot\b/i ) {
        print("Gnuplot File to Generate?\n");
      } elsif( $plot_package =~ /^igor\b/i ) {
        print("Igor File to Generate?\n");
      } elsif( $plot_package =~ /^matlab\b/i ) {
        print("MatLab File to Generate?\n");
      } elsif( $plot_package =~ /^plotmtv\b/i ) {
        print("Plotmtv File to Generate?\n");
      } elsif( $plot_package =~ /^scidavis\b/i ) {
        print("SciDAVis File to Generate?\n");
      }
      $out_file = <STDIN>;
      chomp( $out_file );
      if( open( OUT,">$out_file" ) ) {
        $stops = 1;
      } else {
        print("Error: Unable to Create Plotting-Package File: $out_file. Try again!\n\n");
      }
    } until $stops != 0;
#
#  No second or third(+) argument(s); ask user for STOMP surface file name.
#
    $stops = 1;
    do {
      print("STOMP Surface File Name?\n");
      $in_file = <STDIN>;
      chomp( $in_file );
      if( -r $in_file ) {
        $stops = 1;
      } elsif( -B $in_file )  {
        $stops = 0;
        print("Error: STOMP Surface File is Binary: $in_file[$nf].\n");
      } else {
        $stops = 0;
        print("Error: STOMP Surface File is Unreadable: $in_file[$nf].\n");
      }
      if( $stops == 0 ) { print("Try again!\n\n"); }
    } until $stops != 0;
  }
#
#  No first argument; ask user for plotting package name.
#
} else {
  $stops = 0;
  do {
    print("Plotting Package [Tecplot|Gnuplot|Grapher|Igor|MatLab|SciDAVis|Plotmtv]?\n");
    $plot_package = <STDIN>;
    chomp( $plot_package );
    if( $plot_package =~ /^tecplot\b/i ) {
      print("Plotting Package: Tecplot\n");
      $stops = 1;
    } elsif( $plot_package =~ /^gnuplot\b/i ) {
      print("Plotting Package: Gnuplot\n");
      $stops = 1;
    } elsif( $plot_package =~ /^grapher\b/i ) {
      print("Plotting Package: Grapher\n");
      $stops = 1;
    } elsif( $plot_package =~ /^igor\b/i ) {
      print("Plotting Package: Igor\n");
      $stops = 1;
    } elsif( $plot_package =~ /^matlab\b/i ) {
      print("Plotting Package: MatLab\n");
      $stops = 1;
    } elsif( $plot_package =~ /^plotmtv\b/i ) {
      print("Plotting Package: Plotmtv\n");
      $stops = 1;
    } elsif( $plot_package =~ /^scidavis\b/i ) {
      print("Plotting Package: SciDAVis\n");
      $stops = 1;
    } else {
      print("Error: Unrecognized Plotting Package: $plot_package.  Try again!\n\n");
    }
  } until $stops != 0;
#
#  No first or second argument; ask user for plotting-package file name.
#
  $stops = 0;
  do {
    if( $plot_package =~ /^tecplot\b/i ) {
      print("Tecplot File to Generate?\n");
    } elsif( $plot_package =~ /^grapher\b/i ) {
      print("Grapher File to Generate?\n");
    } elsif( $plot_package =~ /^gnuplot\b/i ) {
      print("Gnuplot File to Generate?\n");
    } elsif( $plot_package =~ /^igor\b/i ) {
      print("Igor File to Generate?\n");
    } elsif( $plot_package =~ /^matlab\b/i ) {
      print("MatLab File to Generate?\n");
    } elsif( $plot_package =~ /^plotmtv\b/i ) {
      print("Plotmtv File to Generate?\n");
    } elsif( $plot_package =~ /^scidavis\b/i ) {
      print("SciDAVis File to Generate?\n");
    }
    $out_file = <STDIN>;
    chomp( $out_file );
    if( open( OUT,">$out_file" ) ) {
      $stops = 1;
    } else {
     print("Error: Unable to Create Plotting-Package File: $out_file. Try again!\n\n");
    }
  } until $stops != 0;
#
#  No first, second or third(+) argument(s); ask user for STOMP surface file name.
#
  $stops = 1;
  do {
    print("STOMP Surface File Name?\n");
    $in_file = <STDIN>;
    chomp( $in_file );
    if( -r $in_file ) {
      $stops = 1;
    } elsif( -B $in_file )  {
      $stops = 0;
      print("Error: STOMP Surface File is Binary: $in_file[$nf].\n");
    } else {
      $stops = 0;
      print("Error: STOMP Surface File is Unreadable: $in_file[$nf].\n");
    }
    if( $stops == 0 ) { print("Try again!\n\n"); }
  } until $stops != 0;
}
#
#  Tecplot Title
#
  if( $plot_package =~ /^tecplot\b/i && $t_opt == 1 ) {
    print("Tecplot Title?\n");
    $title = <STDIN>;
    chomp( $title );
  }
  if( $plot_package =~ /^gnuplot\b/i && $t_opt == 1 ) {
    print("Gnuplot Title?\n");
    $title = <STDIN>;
    chomp( $title );
  }
#
#  Open STOMP surface file
#
open( SURFACE,$in_file ) || die "Error: Unable to Open STOMP Surface File: $in_file.\n";
@surface_array = <SURFACE>;
#
#  Initialize flags
#
$nr = 0;
$vflag = 0;
$aflag = 0;
#
#  Loop over lines in STOMP surface files
#
foreach $surface_line (@surface_array) {
#
#  Remove return from line
#
  chomp( $surface_line );
#
#  Remove leading blank spaces from line
#
  $surface_line =~ s/^\s+//;
#
#  Read the surface-flux variables
#
  if( $vflag == 1 ) {
    @fields = split(/,/,$surface_line);
    if( $fields[0] ) {
      push(@v_name,$fields[0]);
      $fields[1] =~ s/^\s+//;
      if( $fields[1] =~ /null/ ) {
        $fields[1] = '';
      }
      push(@vr_unit,$fields[1]);
      push(@vi_unit,$fields[2]);
    } else {
      $vflag = 2;
#
#     Prompt user for surface-flux variables
#
      if( $av_opt == 0 ) {
        $stops = 0;
        W2: while( $stops == 0 ) {
          print "\nThe STOMP surface file, \"$in_file\", contains the\n";
          print "following surfaces and surface variables:\n\n";
          for( $i = 1; $i <= $#v_name; $i++ ) {
            print "$i --";
            if( $vr_unit[$i] ) {
              print " \"$v_name[$i], $vr_unit[$i]";
              if( $vi_unit[$i] ) {
                print ", $vi_unit[$i]";
              }
              print "\"\n";
            } else {
              print " \"$v_name[$i]\"\n";
            }
          }
          print "a -- all surface-flux variables\n\n";
          print "Enter the surface-flux variables to include in the\n";
          print "plotting-package input file, \"$out_file\", by entering\n";
          print "a string of indices or entering \"a\" for all \n";
          print "surface-flux variables.\n";
          $in_line = <STDIN>;
          chomp( $in_line );
          $in_line =~ s/^\s+//;
          @entries = split(/\s+|,/,$in_line);
          if( $#entries < 0 ) {
            print("Error: No Entries\n");
            print("Try again!\n\n");
            redo W2
          }
          L4: for( $i = 0; $i <= $#entries; $i++ ) {
            if( $entries[$i] =~ /^a\b/i ) {
#
#             Load v_list for the "a" option
#
              push(@v_list,0);
              for( $i = 1; $i <= $#v_name; $i++ ) {
                push(@v_list,$i);
              }
              $av_opt = 1;
              $stops = 1;
              last W2
            }
          }
#
#         Load v_list from user selections
#
          if( $av_opt == 0 ) {
            push(@vx_name,$v_name[0]);
            push(@vx_unit,$v_unit[0]);
            push(@v_list,0);
            L5: for( $i = 0; $i <= $#entries; $i++ ) {
              $found = 0;
              if( $entries[$i] >=0 || $entries[$1] <= $#v_name ) {
                push(@vx_name,$v_name[$entries[$i]]);
                push(@vx_unit,$v_unit[$entries[$i]]);
                push(@v_list,$entries[$i]);
                $found = 1;
              }
              if( $found == 0 ) {
                print("Error: Unrecognized Index: $entries[$i].\n");
                print("Try again!\n\n");
                redo W2
              }
            }
            @v_name = @vx_name;
            @v_unit = @vx_unit;
            $stops = 1;
          }
        }
#
#     Load v_list for the "-a" or "-av" command-line option
#
      } else {
        push(@v_list,0);
        for( $i = 1; $i <= $#v_name; $i++ ) {
          push(@v_list,$i);
        }
      }
#
#     Write surface-flux variables
#
      if( $plot_package =~ /^grapher\b/i ) {
        print OUT "\"simulation time, $t_unit\"";
          for( $i = 1; $i <= $#v_name; $i++ ) {
            if( $v_unit[$i] ) {
              print OUT " \"$v_name[$i] Rate, $v_unit[$i]\"";
              print OUT " \"$v_name[$i] Integral, $v_unit[$i]\"";
            } else {
              print OUT " \"$v_name[$i] Rate\"";
              print OUT " \"$v_name[$i] Integral\"";
            }
          }
        print OUT "\n";
      } elsif( $plot_package =~ /^tecplot\b/i ) {
        print OUT "TITLE = \"$title\"\n";
        print OUT "VARIABLES =";
        print OUT "\"simulation time, $t_unit\"";
          for( $i = 1; $i <= $#v_name; $i++ ) {
            if( $v_unit[$i] ) {
              print OUT " \"$v_name[$i] Rate ($i), $v_unit[$i]\"";
              print OUT " \"$v_name[$i] Integral ($i), $v_unit[$i]\"";
            } else {
              print OUT " \"$v_name[$i] Rate ($i)\"";
              print OUT " \"$v_name[$i] Integral ($i)\"";
            }
          }
        print OUT "\n";
        print OUT "ZONE T=\"Reference Node Variables\" F=POINT\n";
      }
    }
  }
#
#  Read surface-flux variable abbreviations with surface index
#
  if( $plot_package =~ /^igor\b/i  ) {
    if( $vflag == 2 && $aflag == 0 ) {
      $surface_line =~ s/\(/ /g;
      $surface_line =~ s/\)/ /g;
      @fields = split(/\s+/,$surface_line);
      if( $fields[0] =~ /Time\b/ ) {
        $aflag = 1;
        for( $j = 1; $j <= $#fields; $j=$j+2 ) {
          push( @abbrev,"$fields[$j]_$fields[$j+1]" );
        }
#
#       Write surface-flux variable abbreviations with surface index
#
        print OUT "TM";
        for( $i = 0; $i <= $#abbrev; $i++ ) {
          print OUT " $abbrev[$i]";
        }
        print OUT "\n";
      }
    }
  }
#
#  Read surface-flux variable abbreviations with surface index
#
  if( $plot_package =~ /^scidavis\b/i  ) {
    if( $vflag == 2 && $aflag == 0 ) {
      $surface_line =~ s/\(/ /g;
      $surface_line =~ s/\)/ /g;
      @fields = split(/\s+/,$surface_line);
      if( $fields[0] =~ /Time\b/ ) {
        $aflag = 1;
        for( $j = 1; $j <= $#fields; $j=$j+2 ) {
          push( @abbrev,"$fields[$j]_$fields[$j+1]" );
        }
#
#       Write surface-flux variable abbreviations with surface index
#
        print OUT "TM";
        for( $i = 0; $i <= $#abbrev; $i++ ) {
          print OUT ",$abbrev[$i]";
        }
        print OUT "\n";
      }
    }
  }
#
#  Read surface-flux variable abbreviations with surface index
#
  if( $plot_package =~ /^gnuplot\b/i ) {
    if( $vflag == 2 && $aflag == 0 ) {
      $surface_line =~ s/\(/ /g;
      $surface_line =~ s/\)/ /g;
      @fields = split(/\s+/,$surface_line);
      if( $fields[0] =~ /Time\b/ ) {
        $aflag = 1;
        for( $j = 1; $j <= $#fields; $j=$j+2 ) {
          $fields[$j] =~ tr/A-Z/a-z/;
          push( @abbrev,"$fields[$j]_$fields[$j+1]" );
        }
      }
    }
  }
#
#  Read surface-flux variable abbreviations with surface index
#
  if( $plot_package =~ /^plotmtv\b/i ) {
    if( $vflag == 2 && $aflag == 0 ) {
      $surface_line =~ s/\(/ /g;
      $surface_line =~ s/\)/ /g;
      @fields = split(/\s+/,$surface_line);
      if( $fields[0] =~ /Time\b/ ) {
        $aflag = 1;
        for( $j = 1; $j <= $#fields; $j=$j+2 ) {
          $fields[$j] =~ tr/A-Z/a-z/;
          push( @abbrev,"$fields[$j]_$fields[$j+1]" );
        }
      }
    }
  }
#
#  Read and write the surface-flux record
#
  if( $vflag == 2 ) {
    @fields = split(/\s+/,$surface_line);
    if( $fields[0] =~ /^[0-9]/ ) {
#
#     Load all surface flux variables
#
      if( $av_opt == 1 ) {
#
#       Fill record array for plotmtv plotting package
#
        if( $plot_package =~ /^plotmtv\b/i ) {
          foreach $junk (@fields) {
            push( @record,$junk );
          }
        } else {
          @record = @fields
        }
#
#     Load selected surface flux variables
#
      } else {
        push( @record,$fields[0] );
        for( $i = 1; $i <= $#v_list; $i++ ) {
          $ir = ($v_list[$i]-1)*2 + 1;
          $jr = ($v_list[$i]-1)*2 + 2;
#          print OUT "ir = $ir jr = $jr\n";
          push( @record,$fields[$ir] );
          push( @record,$fields[$jr] );
        }
      }
#
#     Delay writing record array for plotmtv plotting package
#
      unless( $plot_package =~ /^plotmtv\b/i ) {
#
#       Write record
#
        if( $plot_package =~ /^scidavis\b/i ) {
          for( $i = 0; $i <= $#record; $i++ ) {
            print OUT "$record[$i],";
          }
        } else {
          for( $i = 0; $i <= $#record; $i++ ) {
            print OUT "$record[$i] ";
          }
        }
        print OUT "\n";
        @record = ();
      }
    }
  }
#
#  Read simulation time units
#
  if( $vflag == 2 && $surface_line =~ /^Time/ ) {
    @fields = split(/,/,$surface_line);
    $t_unit = $fields[1];
  }
#
#  Start reading surface variables, skipping one blank line
#
  if( $vflag == -1 ) {
    $vflag = 1;
  }
  if( $surface_line =~ /Surface Variables/ ) {
    $vflag = -1;
  }
}
#
#  Close surface file (input)
#
close( SURFACE );
#
#  Write output for plotmtv plotting package
#
if( $plot_package =~ /^plotmtv\b/i ) {
  print OUT ("\$ DATA=CURVE2D\n\n");
  $nvar = $#v_list*2+1;
  $nrecord = $#record+1;
  $ntime = $nrecord/$nvar;
#  print OUT ("number of time steps = $ntime\n");
  for( $j = 0; $j < $#v_list; $j++ ) {
    $jx = $j+1;
    $iv = ($v_list[$j+1]-1)*2;
    print OUT ("% toplabel = \"Surface Data\"\n");
    print OUT ("% subtitle = \"Rates\"\n");
    print OUT ("% xlabel = \"Time, $t_unit\"\n");
    if( $v_unit[$i] ) {
        print OUT ("% ylabel = \"$v_name[$j], $v_unit[$j]\"\n");
    } else {
        print OUT ("% ylabel = \"$v_name[$j]\"\n");
    }
      print OUT ("#% xmin = \n");
      print OUT ("#% xmax = \n");
      print OUT ("#% ymin = \n");
      print OUT ("#% ymax = \n");
      print OUT ("% equalscale = false\n");
      print OUT ("% fitpage = true\n\n");
    
#    print OUT ("% linetype = $jx\n");
    print OUT ("% linecolor = $jx\n");
    print OUT ("% markertype = $jx\n");
    print OUT ("% linelabel = $abbrev[$iv]\n");
    for( $i = 0; $i < $ntime; $i++ ) {
      $ir = $i*$nvar;
      $jr = $ir + $j*2 + 1;
      print OUT "$record[$ir]  $record[$jr]\n";
    }
    print OUT "\n";
  }
  print OUT ("\$ DATA=CURVE2D\n\n");
  print OUT ("% toplabel = \"Surface Data\"\n");
  print OUT ("% subtitle = \"Integrals\"\n");
  for( $j = 0; $j < $#v_list; $j++ ) {
    $jx = $j+1;
    print OUT ("% xlabel = \"Time, $t_unit\"\n");
    if( $v_unit[$i] ) {
        print OUT ("% ylabel = \"$v_name[$j], $v_unit[$j]\"\n");
    } else {
        print OUT ("% ylabel = \"$v_name[$j]\"\n");
    }
     print OUT ("#% xmin = \n");
      print OUT ("#% xmax = \n");
      print OUT ("#% ymin = \n");
      print OUT ("#% ymax = \n");
      print OUT ("% equalscale = false\n");
      print OUT ("% fitpage = true\n\n");
      
#    print OUT ("% linetype = $jx\n");
    print OUT ("% linecolor = $jx\n");
    print OUT ("% markertype = $jx\n");
    $iv = ($v_list[$j+1]-1)*2 + 1;
    print OUT ("% linelabel = $abbrev[$iv]\n");
    for( $i = 0; $i < $ntime; $i++ ) {
      $ir = $i*$nvar;
      $jr = $ir + $j*2 + 2;
      print OUT "$record[$ir]  $record[$jr]\n";
    }
    print OUT "\n";
  }
}
#
#  Close plotting package file (output)
#
close( OUT );
#
#  Write gnuplot scripts for each reference node variable
#
if( $plot_package =~ /^gnuplot\b/i ) {
  for( $i = 1; $i <= $#v_name; $i++ ) {
#
#   Rate variable
#
    $k = ($i-1)*2;
    $gnu_file = $abbrev[$k] . "_surf.gnu";
    open( GNU,">$gnu_file") || die "Error: Unable to Open Gnuplot Script File: $gnu_file.\n";
    print GNU "set term x11 font \"Times,16\"\n";
    print GNU "set xlabel \"Time, $t_unit\"\n";
    if( $title ) {
      print GNU "set title \"$title\"\n";
    }
    if( $vr_unit[$i] ) {
      print GNU "set ylabel \"$v_name[$i], $vr_unit[$i]\"\n";
    } else {
      print GNU "set ylabel \"$v_name[$i]\"\n";
    }
    print GNU "set key outside\n";
    if( $logx_opt ) {
      print GNU "set log x\n";
    }
    if( $logy_opt ) {
      print GNU "set log y\n";
    }
    print GNU "plot [:] [:] \\\n";
    $k = $k+2;
    print GNU "\'$out_file\' using 1:$k with lines lw 2\n";
    print GNU "pause mouse \"Click to continue ...\"\n";
    print GNU "reset\n";
    close( GNU );
#
#   Integral variable
#
    $k = ($i-1)*2 + 1;
    $gnu_file = $abbrev[$k] . "_surf.gnu";
    open( GNU,">$gnu_file") || die "Error: Unable to Open Gnuplot Script File: $gnu_file.\n";
    print GNU "set term x11 font \"Times,16\"\n";
    print GNU "set xlabel \"Time, $t_unit\"\n";
    if( $title ) {
      print GNU "set title \"$title\"\n";
    }
    if( $vr_unit[$i] ) {
      print GNU "set ylabel \"$v_name[$i], $vi_unit[$i]\"\n";
    } else {
      print GNU "set ylabel \"$v_name[$i]\"\n";
    }
    print GNU "set key outside\n";
    if( $logx_opt ) {
      print GNU "set log x\n";
    }
    if( $logy_opt ) {
      print GNU "set log y\n";
    }
    print GNU "plot [:] [:] \\\n";
    $k = $k+2;
    print GNU "\'$out_file\' using 1:$k with lines lw 2\n";
    print GNU "pause mouse \"Click to continue ...\"\n";
    print GNU "reset\n";
    close( GNU );
  }
}

