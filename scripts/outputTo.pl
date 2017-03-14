#!/usr/bin/perl
# $Id: outputTo.pl 1080 2017-03-14 16:22:02Z d3c002 $
#
#  Print banner.
#
print("\nWelcome to outputTo ...\n\n");
print("This perl program transforms a STOMP output file into\n");
print("formatted input file for Gnuplot, Grapher, Igor, MatLab, Plotmtv, Tecplot, and SciDAVis.\n");
print("Entries not made on the command line will be prompted.\n\n");
#
#  Initialize variables.
#
$nargv = $#ARGV;
$t_opt = 0;
$ts_opt = 0;
$an_opt = 0;
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
    print "    Plotting Package Option [Gnuplot|Grapher|Igor|MatLab|Plotmtv|Tecplot|SciDAVis]\n";
    print "    Plotting Package File to Generate\n";
    print "    STOMP Output File Name\n\n";
    print "Options:\n\n";
    print "-help      this help\n";
    print "-a         all nodes, all variables\n";
    print "-an        all nodes\n";
    print "-av        all variables\n";
    print "-t         title prompt option (Tecplot and Gnuplot only)\n";
    print "-logx      plot time on logarithmic scale (Gnuplot only)\n";
    print "-logy      plot variables on logarithmic scale (Gnuplot only)\n\n";
    print "Plotting Package Options:\n\n";
    print "Gnuplot\n";
    print "Grapher\n";
    print "Tecplot\n";
    print "Igor\n\n";
    print "Plotmtv\n";
    print "MatLab\n\n";
    print "SciDAVis\n\n";
    print "Examples:\n\n";
    print "outputTo.pl -an -t Gnuplot output.dat output \n";
    print "  -- prints all nodes (variables and title will be prompted) to file output.dat\n";
    print "outputTo.pl -av -an Tecplot ref_nodes.dat output \n";
    print "  -- prints all nodes and variables to file ref_nodes.dat\n";
    print "outputTo.pl Grapher ref_nodes.dat output\n";
    print "  -- prints (variables and nodes will be prompted) to file ref_nodes.dat\n";    
    print "outputTo.pl -a plotmtv plotmtv.dat output \n";
    print "  -- prints all nodes and all variables to file plotmtv.dat\n";
    print "outputTo.pl plotmtv aqueous_pressure.dat output \n";
    print "  -- prints (variables and nodes will be prompted) to file aqueous_pressure.dat\n";        
    print "outputTo.pl -av -t Tecplot tecplot.dat output\n";
    die   "  -- prints all variables (nodes and title will be prompted) to file tecplot.dat\n\n";
#
#  All nodes option
#
  } elsif( $ARGV[0] =~ /\-an\b/ ) {
    $an_opt = 1;
    shift(@ARGV);
#
#  All variables option
#
  } elsif( $ARGV[0] =~ /\-av\b/i ) {
    $av_opt = 1;
    shift(@ARGV);
#
#  All nodes, all variables option
#
  } elsif( $ARGV[0] =~ /\-a\b/i ) {
    $an_opt = 1;
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
  } elsif( $plot_package =~ /SciDAVis\b/i ) {
    print("Plotting Package: SciDAVis\n");
  } else {
    die "Error: Unrecognized Plotting Package: $plot_package.\n\n";
  }
#
#  Search for plotting-package file name as second argument or prompt user.
#
  if( $ARGV[1] ) {
    $out_file = $ARGV[1];
    chomp( $out_file );
    open( OUT,">$out_file") || die "Error: Unable to Create Plotting Package File: $out_file.\n";
#
#  Search for STOMP output file as third argument or prompt user.
#
    if( $ARGV[2] ) {
#
#  Check for multiple plotting-package input files
#
      if( $#ARGV > 2 ) {
        die "Error: Multiple STOMP Output Files Specified.\n";
      }
      $in_file = $ARGV[2];
#      print "STOMP Output File: $in_file\n";
#
#  No third(+) argument; ask user for STOMP output file name.
#
    } else {
      $stops = 1;
      do {
        print("STOMP Output File Name?\n");
        $in_file = <STDIN>;
        chomp( $in_file );
        if( -r $in_file ) {
          $stops = 1;
        } elsif( -B $in_file )  {
          $stops = 0;
          print("Error: STOMP Output File is Binary: $in_file[$nf].\n");
        } else {
          $stops = 0;
          print("Error: STOMP Output File is Unreadable: $in_file[$nf].\n");
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
        print("Error: Unable to Create Plotting Package File: $out_file. Try again!\n\n");
      }
    } until $stops != 0;
#
#  No second or third(+) argument(s); ask user for STOMP output file name.
#
    $stops = 1;
    do {
      print("STOMP Output File Name?\n");
      $in_file = <STDIN>;
      chomp( $in_file );
      if( -r $in_file ) {
        $stops = 1;
      } elsif( -B $in_file )  {
        $stops = 0;
        print("Error: STOMP Output File is Binary: $in_file[$nf].\n");
      } else {
        $stops = 0;
        print("Error: STOMP Output File is Unreadable: $in_file[$nf].\n");
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
    print("Plotting Package [Gnuplot|Grapher|Igor|Matlab|Plotmtv|Tecplot|SciDAVis]?\n");
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
     print("Error: Unable to Open Plotting Package File: $out_file. Try again!\n\n");
    }
  } until $stops != 0;
#
#  No first, second or third(+) argument(s); ask user for STOMP output file name.
#
  $stops = 1;
  do {
    print("STOMP Output File Name?\n");
    $in_file = <STDIN>;
    chomp( $in_file );
    if( -r $in_file ) {
      $stops = 1;
    } elsif( -B $in_file )  {
      $stops = 0;
      print("Error: STOMP Output File is Binary: $in_file[$nf].\n");
    } else {
      $stops = 0;
      print("Error: STOMP Output File is Unreadable: $in_file[$nf].\n");
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
#  Open STOMP output file
#

open( OUTPUT,$in_file ) || die "Error: Unable to Open STOMP Output File: $in_file.\n";
@output_array = <OUTPUT>;

#
#  Initialize flags
#
$nv = 0;
$nn = 0;
$nr = 0;
$vflag = 0;
$aflag = 0;
$nflag = 0;
$rflag = 0;
$cflag = 0;
$sflag = 0;
$snum = 0;
#
#  Loop over lines in STOMP output files
#
foreach $output_line (@output_array) {
#
#  Remove return from line
#
  chomp( $output_line );
#
#  Remove leading blank spaces from line
#
  $output_line =~ s/^\s+//;
#
#  Read the coordinate system dimensions
#
  if( $cflag >= 1 ) {
    @fields = split(/\s+/,$output_line);
    if( $fields[0] ) {
      if( $cflag == 1 ) {
        $ifld = $fields[4];
      } elsif( $cflag == 2 ) {
        $jfld = $fields[4];
      } elsif( $cflag == 3 ) {
        $kfld = $fields[4];
      }
      $cflag++;
    } else {
        $cflag = 0;
    }
  }
#
#  Read the reference-node indices
#
  if( $nflag == 1 ) {
    @fields = split(/\s+/,$output_line);

    if( $fields[0] ) {
#      $nn++;
      push(@in,$fields[6]);
      push(@jn,$fields[9]);
      push(@kn,$fields[12]);
      $node = ($fields[12]-1)*($ifld*$jfld) + ($fields[9]-1)*$ifld + $fields[6];
      push(@nn,$node);
    } else {
      $nflag = 0;
#
#  Prompt user for reference-node indices
#
    if( $an_opt == 0 ) {
      $stops = 0;
      W1: while( $stops == 0 ) {
          print "The STOMP output file, \"$in_file\", contains the\n";
          print "following reference-nodes:\n\n";
          for( $i = 0; $i <= $#nn; $i++ ) {
            print "$nn[$i] -- ($in[$i],$jn[$i],$kn[$i])\n";
          }
          print "a -- all reference nodes\n\n";
          print "Enter the reference nodes to include in the\n";
          print "plotting-package input file, \"$out_file\", by entering\n";
          print "a string of indices or entering \"a\" for all \n";
          print "reference nodes.\n";
          $in_line = <STDIN>;
          chomp( $in_line );
          $in_line =~ s/^\s+//;
          @entries = split(/\s+|,/,$in_line);
          if( $#entries < 0 ) {
            print("Error: No Entries\n");
            print("Try again!\n\n");
            redo W1
          }
          L1: for( $i = 0; $i <= $#entries; $i++ ) {
            if( $entries[$i] =~ /^a\b/i ) {
              $an_opt = 1;
              $stops = 1;
              last W1
            }
          }
          if( $an_opt == 0 ) {
            L2: for( $i = 0; $i <= $#entries; $i++ ) {
              $found = 0;
              L3: for( $j = 0; $j <= $#nn; $j++ ) {
                if( $entries[$i] == $nn[$j] ) {
                  push(@nnx,$nn[$j]);
                  push(@inx,$in[$j]);
                  push(@jnx,$jn[$j]);
                  push(@knx,$kn[$j]);
                  $found = 1;
                  last L3
                }
              }
              if( $found == 0 ) {
                print("Error: Unrecognized Index: $entries[$i].\n");
                print("Try again!\n\n");
                redo W1
              }
            }
            @nn = @nnx;
            @in = @inx;
            @jn = @jnx;
            @kn = @knx;
            $stops = 1;
          }
        }
      }
    }
  }
#
#  Read the reference-node variables
#
  if( $vflag == 1 ) {
    @fields = split(/,/,$output_line);
    if( $fields[0] )
    {
      $nv++;
      push(@v_name,$fields[0]);
      $fields[1] =~ s/^\s+//;

      if ($fields[1] =~ /null/ ) {
        if ($snum <=1) {
          $fields[1] = '';
        } 
        if ($snum >1  && $fields[0] =~ /solute integrated/) {
          @fields2 = split(/s+/,$fields[1]);
          if ($fields2[0] =~ /null/) {
            $fields[1] = substr($fields[1],5);
          }
        }   
      }

      push(@v_unit,$fields[1]);
    }
    else
    {
      $vflag = 0;
#
#     Prompt user for reference-node variables
#
      if( $av_opt == 0 ) {
        $stops = 0;
        W2: while( $stops == 0 ) {
          print "The STOMP output file, \"$in_file\", contains the\n";
          print "following reference-node variables:\n\n";
          for( $i = 0; $i <= $#v_name; $i++ ) {
            print "$i --";
            if( $v_unit[$i] ) {
              print " \"$v_name[$i], $v_unit[$i]\"\n";
            } else {
              print " \"$v_name[$i]\"\n";
            }
          }
          print "a -- all reference-node variables\n\n";
          print "Enter the reference-node variables to include in the\n";
          print "plotting-package input file, \"$out_file\", by entering\n";
          print "a string of indices or entering \"a\" for all \n";
          print "reference node variables.\n";
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
              $av_opt = 1;
              $stops = 1;
              for( $i = 0; $i <= $#v_name; $i++ ) {
                push(@v_list,$i);
              }
              last W2;
            }
          }
          if( $av_opt == 0 ) {
            L5: for( $i = 0; $i <= $#entries; $i++ ) {
              $found = 0;
              if( !($entries[$i] =~ /[0-9]+/i))
              {
                  print ("Error: Unrecognized Index: $entries[$i].\n\n");
                  redo W2;
              }

              if( $entries[$i] >=0 && $entries[$1] <= $#v_name ) {
                push(@vx_name,$v_name[$entries[$i]]);
                push(@vx_unit,$v_unit[$entries[$i]]);
                push(@v_list,$entries[$i]);
                $found = 1;
              }
              if( $found == 0 ) {
                print("Error: Unrecognized Index: $entries[$i].\n\n");
                print("Try again!\n\n");
                redo W2;
              }
            }
            @v_name = @vx_name;
            @v_unit = @vx_unit;
            $stops = 1;
          }
        }
      }
      else
      {
        for( $i = 0; $i <= $#v_name; $i++ ) {
          push(@v_list,$i);
        }
      }

#
#     Write reference-node variables with node index
#
      if( $plot_package =~ /^grapher\b/i ) {
        print OUT "\"simulation time, $t_unit\"";
        for( $j = 0; $j <= $#nn; $j++ ) {
          for( $i = 0; $i <= $#v_name; $i++ ) {
            if( $v_unit[$i] ) {
              print OUT " \"$v_name[$i] ($nn[$j]), $v_unit[$i]\"";
            } else {
              print OUT " \"$v_name[$i] ($nn[$j])\"";
            }
          }
        }
        print OUT "\n";
      } elsif( $plot_package =~ /^gnuplot\b/i ) {
        print OUT "#\"simulation time, $t_unit\"";
        for( $j = 0; $j <= $#nn; $j++ ) {
          for( $i = 0; $i <= $#v_name; $i++ ) {
            if( $v_unit[$i] ) {
              print OUT " \"$v_name[$i] ($nn[$j]), $v_unit[$i]\"";
            } else {
              print OUT " \"$v_name[$i] ($nn[$j])\"";
            }
          }
        }
        print OUT "\n";
      } elsif( $plot_package =~ /^tecplot\b/i ) {
        print OUT "TITLE = \"$title\"\n";
        print OUT "VARIABLES =";
        print OUT "\"simulation time, $t_unit\"";
        $len = 30 + length($t_unit);
        for( $j = 0; $j <= $#nn; $j++ ) {
          for( $i = 0; $i <= $#v_name; $i++ ) {
            if( $v_unit[$i] ) {
              print OUT " \"$v_name[$i] ($nn[$j]), $v_unit[$i]\"";
              $len = $len + length($v_name[$i]) + length($nn[$j]) + length($v_unit[$i]) + 8;
            } else {
              print OUT " \"$v_name[$i] ($nn[$j])\"";
              $len = $len + length($v_name[$i]) + length($nn[$j]) + 6;
            }
          }
        }
        if ($len>31998) {
          print "\nWarning: The Variable line (the second line) in file \"$out_file\" is too long.\n";
          print "          (The maximun number of characters is 31998, you have $len characters) \n";
          print "          Tecplot can not load it. \n\n";
        }
        print OUT "\n";
        print OUT "ZONE T=\"Reference Node Variables\" F=POINT\n";
      }
    }
  }
#
#  Read reference-node variable abbreviations
#
  if( $plot_package =~ /^igor\b/i ) {
    if( $rflag == 1 && $aflag == 0) {
      @fields = split(/\s+/,$output_line);
      if( $fields[0] =~ /Step\b/ ) {
        $aflag = 1;
        for( $j = 6; $j <= $#fields; $j++ ) {
          push( @abbrev,$fields[$j] );
        }
#
#       Write reference-node variable abbreviations with node index
#
        print OUT "TM";
        for( $j = 0; $j <= $#nn; $j++ ) {
          for( $i = 0; $i <= $#v_list; $i++ ) {
            print OUT " $abbrev[$v_list[$i]]_$nn[$j]";
          }
        }
        print OUT "\n";
      }
    }
  }
#
#  Read reference-node variable abbreviations
# 
    if( $plot_package =~ /^scidavis\b/i ) {
    if( $rflag == 1 && $aflag == 0) {
      @fields = split(/\s+/,$output_line);
      if( $fields[0] =~ /Step\b/ ) {
        $aflag = 1;
        for( $j = 6; $j <= $#fields; $j++ ) {
          push( @abbrev,$fields[$j] );
        }
#
#       Write reference-node variable abbreviations with node index
#
        print OUT "TM";
        for( $j = 0; $j <= $#nn; $j++ ) {
          for( $i = 0; $i <= $#v_list; $i++ ) {
            print OUT ",$abbrev[$v_list[$i]]_$nn[$j]";
          }
        }
        print OUT "\n";
      }
    }
  }
#
#  Read reference-node variable abbreviations for gnuplot
#
  if( $plot_package =~ /^gnuplot\b/i ) {
    if( $rflag == 1 && $aflag == 0) {
      @fields = split(/\s+/,$output_line);
      if( $fields[0] =~ /Step\b/ ) {
        $aflag = 1;
        for( $j = 6; $j <= $#fields; $j++ ) {
          $fields[$j] =~ tr/A-Z/a-z/;
          push( @abbrev,$fields[$j] );
        }
      }
    }
  }
#
#  Read reference-node variable abbreviations for plotmtv
#
  if( $plot_package =~ /^plotmtv\b/i ) {
    if( $rflag == 1 && $aflag == 0) {
      @fields = split(/\s+/,$output_line);
      if( $fields[0] =~ /Step\b/ ) {
        $aflag = 1;
        for( $j = 6; $j <= $#fields; $j++ ) {
          $fields[$j] =~ tr/A-Z/a-z/;
          push( @abbrev,$fields[$j] );
        }
      }
    }
  }
#
#  Read the reference-node records
#
  if( $rflag == 1 ) {
    @fields = split(/\s+/,$output_line);
    $mulsolute=0;
    $startNum=0;
    if ($fields[0]=~/[0-9]/) {
    if ($fields[4]=~/\[/) {
      $mulsolute=1;
      for( $i = 6; $i <= $#fields; $i++ ) {
        if ($fields[$i] =~/\]/) {
          $startNum=$i+1;
          last;
        }
      }
    }
    for( $j = 0; $j <= $#nn; $j++ ) {
      if( $fields[1] == $nn[$j] ) {
#
#       Start loading new record, include simulation time and
#       variable values in record
#
        if ( $nr == 0 ) {
          push( @record,$fields[2] );
          if ($av_opt == 1) {
            $s=0;
            if ($mulsolute==0) {
              $s=6;
            } else {
              $s=$startNum;
            }
            for( $i = $s; $i <= $#fields; $i++ ) {
              push( @record,$fields[$i] );
            }
          } else {
            for( $i = 0; $i <= $#v_list; $i++ ) {
              if ($mulsolute==0) {
                push( @record,$fields[$v_list[$i]+5] );
              } else {
                push( @record,$fields[$v_list[$i]+$startNum]);      
              }      
            }
          }
#
#       Continue loading new record, include only variable
#       values in record
#
        } else {
          if ($av_opt == 1) {
            $s=0;
            if ($mulsolute==0) {
              $s=6;
            } else {
              $s=$startNum;
            }
            for( $i = $s; $i <= $#fields; $i++ ) {
              push( @record,$fields[$i] );
            }
          } else {
            for( $i = 0; $i <= $#v_list; $i++ ) {
              if ($mulsolute==0) {
                push( @record,$fields[$v_list[$i]+5] );
              } else {
                push( @record,$fields[$v_list[$i]+$startNum]);      
              }      
            }
          }
        }
        $nr++;
#
#       All reference nodes loaded write reference-node record
#       and reset record
#
        if( $nr > $#nn ) {
          $nr = 0;
#
#         Write reference-node record and reset record for all plotting
#         packages except plotmtv
#
          unless( $plot_package =~ /^plotmtv\b/i ) {
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
    }
  }
  }
#
#  Start reading coordinate system
#
  if( $output_line =~ /Coordinate System Dimensions/ ) {
    $cflag = 1;
  }
#
#  Read simulation time units
#
  if( $output_line =~ /Time Output Units/ ) {
    @fields = split(/\s+/,$output_line);
    $t_unit = $fields[3];
  }
#
#  Counting the number of solutes 
#
  if ( $output_line =~ /~ Solute\/Fluid Interaction Card:/) {
    $sflag =1;
  }
  if ($sflag == 1) {
    $snum ++ if ($output_line =~ /^Solute Name:/);
    $sflag =0 if ($output_line=~ /Solute\/Porous Media Interaction Card:/);
  } 
#
#  Start reading reference-node indices on the next line
#
  if( $output_line =~ /Reference Node No. and Indices/ ) {
    $nflag = 1;
  }
#
#  Start reading reference-node variables on the next line
#
  if( $output_line =~ /Reference Node Variables/ ) {
    $vflag = 1;
  }
#
#  Start reading reference-node record
#
  if( $output_line =~ /---  Reference Node Output Record  ---/ ) {
    $rflag = 1;
  }
}
#
#  Close STOMP output file
#
close( OUTPUT );
#
#  Write output for plotmtv plotting package
#
if( $plot_package =~ /^plotmtv\b/i ) {
#
#  Number of time steps
#
  $nrecord = $#record+1;
  $nvar = $#v_list+1;
  $nnodes = $#nn+1;
#  print OUT ("number of records = $nrecord\n");
#  print OUT ("number of variables = $nvar\n");
#  print OUT ("number of reference nodes = $nnodes\n");
  $ntime = $nrecord/(($nvar*$nnodes)+1);
#  print OUT ("number of time steps = $ntime\n");
#
#  Loop through the variables
#
  for( $i = 0; $i <= $#v_list; $i++ ) {
    print OUT ("\$ DATA=CURVE2D\n");
#
#   Loop through the reference nodes
#
    for( $j = 0; $j <= $#nn; $j++ ) {
      if( $v_unit[$i] ) {
        print OUT ("% toplabel = \"$v_name[$i], $v_unit[$i]\"\n");
      }   else {
        print OUT ("% toplabel = \"$v_name[$i]\"\n");
      }
#      print OUT ("% subtitle = \"Reference Node ($in[$j],$jn[$j],$kn[$j]: $nn[$j])\"\n");
      print OUT ("% subtitle = \"At Reference Nodes\"\n");
      print OUT ("% xlabel = \"Time, $t_unit\"\n");
      if( $v_unit[$i] ) {
        print OUT ("% ylabel = \"$v_name[$i], $v_unit[$i]\"\n");
    }   else {
        print OUT ("% ylabel = \"$v_name[$i]\"\n");
    }
      print OUT ("#% xmin = \n");
      print OUT ("#% xmax = \n");
      print OUT ("#% ymin = \n");
      print OUT ("#% ymax = \n");
      print OUT ("% equalscale = false\n");
      print OUT ("% fitpage = true\n\n");

#    
      $jx = $j+1;
#      print OUT ("% linetype = $jx\n");
      print OUT ("% linecolor = $jx\n");
      print OUT ("% linelabel = \"Node $nn[$j]\"\n");
#      print OUT ("% markertype = $jx\n");
#
#     Loop over number of time records
#
      for( $k = 0; $k < $ntime; $k++ ) {
        $ir = $k*(($nvar*$nnodes)+1);
        $jr = $ir + $j*$nvar + $i + 1;
        print OUT ("$record[$ir] $record[$jr]\n");
      }
      print OUT "\n";
    }
  }
#  for( $i = 0; $i <= $#record; $i++ ) {
#    print OUT ("$record[$i]\n");
#  }
}
#
#  Close plotting package input file
#
close( OUT );
#
#  Write gnuplot scripts for each reference node variable
#
if( $plot_package =~ /^gnuplot\b/i ) {
  for( $i = 0; $i <= $#v_name; $i++ ) {
    $gnu_file = $abbrev[$i] . "_out.gnu";
    open( GNU,">$gnu_file") || die "Error: Unable to Open Gnuplot Script File: $gnu_file.\n";
    print GNU "set term x11 font \"Times,16\"\n";
    print GNU "set xlabel \"Time, $t_unit\"\n";
    if( $title ) {
      print GNU "set title \"$title\"\n";
    }
    if( $v_unit[$i] ) {
      print GNU "set ylabel \"$v_name[$i], $v_unit[$i]\"\n";
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
    for( $j = 0; $j <= $#nn; $j++ ) {  
      $k = $j*($#v_name+1) + $i + 2;
      if( $j < $#nn ) {
        print GNU "\'$out_file\' using 1:$k title \"Node $nn[$j]\" with lines lw 2, \\\n";
      } else {
        print GNU "\'$out_file\' using 1:$k title \"Node $nn[$j]\" with lines lw 2\n";
      }
    }
    print GNU "pause mouse \"Click to continue ...\"\n";
    print GNU "reset\n";
    close( GNU );
  }
}

