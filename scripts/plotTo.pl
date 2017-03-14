#!/usr/bin/perl
# $Id: plotTo.pl 1080 2017-03-14 16:22:02Z d3c002 $
print("\nWelcome to plotTo ...\n\n");
print("This perl program transforms STOMP plot file(s) into\n");
print("formatted input files for Gnuplot, Matlab, Plotmtv, Surfer, Tecplot, VTK, and SCiDAVis.\n");
print("VTK format can be read by VisIt and Paraview.\n");
print("Multiple plot files are allowable for Gnuplot, Plotmtv and Tecplot;\n");
print("whereas, Matlab, Surfer, VTK, and SciDAVis only accept a single plot file.\n");
print("Entries not made on the command line will be prompted.\n\n");
$nargv = $#ARGV;
#
#  Check command line for options.
#
$t_opt = 0;
$ts_opt = 0;
$ta_opt = 0;
$res_opt = 0;
$logx_opt = 0;
$logy_opt = 0;
$rdfv_opt = 0;
$flipxy_opt =0;
while ( $ARGV[0] =~ /^-/i ) {
  if( $ARGV[0] =~ /help\b/i ) {
    print "Command Line Entry:\n\n";
    print "    Plotting Package Option [Gnuplot|MatLab|Plotmtv|Surfer|Tecplot|TecplotXY|Tecplot10|VTK|SciDAVis]\n";
    print "    PLotting Package File to Generate\n";
    print "    STOMP Plot File Name(s)\n\n";
    print "Options:\n\n";
    print "-help      this help\n";
    print "-x         prompt for Gnuplot grid resolution factor\n";
    print "-t         prompt for Gnuplot or Tecplot title\n";
    print "-ts        use time stamp for Gnuplot or Tecplot zone names\n";
    print "-logx      plot x-axis on logarithmic scale (Gnuplot only)\n";
    print "-logy      plot y-axis on logarithmic scale (Gnuplot only)\n";
    print "-flipxy    Flip x- and y-axes (Plotmtv only)\n\n";
    print "Plotting Package Options:\n\n";
    print "Gnuplot\n";
    print "Matlab\n";
    print "Plotmtv\n";
    print "Surfer\n";
    print "Tecplot (for contour plots)\n";
    print "TecplotXY (for XY Line plots on 1-D grids)\n";
    print "Tecplot10 (for Tecplot Version 10 and older)\n";
    print "VTK\n";
    print "SciDAVis\n";
    print "Examples:\n\n";
    print "plotTo.pl -t -ts Tecplot plots.dat plot.123 plot.456 plot.789\n";
    print "plotTo.pl -t Gnuplot plots.dat plot.[0-9][0-9]\n";
    print "plotTo.pl Surfer plot.dat plot.123\n";
    print "plotTo.pl plotmtv aqueous_density.dat plot.123 plot.456 plot.789\n";
    print "plotTo.pl plotmtv aqueous_saturation.dat `ls -1rt plot.*`\n";
    print "plotTo.pl Tecplot plots.dat plot.*\n";
    print "plotTo.pl TecplotXY plots.dat plot.*\n";
    die "plotTo.pl -t Tecplot plots.dat `ls -1rt plot.*`\n\n";
#
#  Tecplot or Gnuplot title option
#
  } elsif( $ARGV[0] =~ /\-t\b/ ) {
    $t_opt = 1;
    shift(@ARGV);
#
#  Tecplot time adjust option
#
  } elsif( $ARGV[0] =~ /\-t\b/ ) {
    $ta_opt = 1;
    shift(@ARGV);
#
#  Tecplot or Gnuplot time stamp option
#
  } elsif( $ARGV[0] =~ /\-ts\b/ ) {
    $ts_opt = 1;
    shift(@ARGV);
#
#  Gnuplot grid resolution option
#
  } elsif( $ARGV[0] =~ /\-x\b/ ) {
    $res_opt = 1;
    shift(@ARGV);
#
#  Gnuplot grid resolution option
#
  } elsif( $ARGV[0] =~ /\-logx\b/ ) {
    $logx_opt = 1;
    shift(@ARGV);
#
#  Gnuplot grid resolution option
#
  } elsif( $ARGV[0] =~ /\-logy\b/ ) {
    $logy_opt = 1;
    shift(@ARGV);
#
#  Flipx x- and y-axes option
#
  } elsif( $ARGV[0] =~ /\-flipxy\b/ ) {
    $flipxy_opt = 1;
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
#  chomp( $plot_package );
  if( $plot_package =~ /\n/ ) {
    chop( $plot_package );
    print "$plot_package\n";
  }
  if( $plot_package =~ /^tecplot\b/i ) {
    print("Plotting Package: Tecplot\n");
  } elsif( $plot_package =~ /^tecplot10\b/i ) {
    print("Plotting Package: Tecplot10\n");
  } elsif( $plot_package =~ /^tecplotxy\b/i ) {
    print("Plotting Package: TecplotXY\n");
  } elsif( $plot_package =~ /^surfer\b/i ) {
    print("Plotting Package: Surfer\n");
  } elsif( $plot_package =~ /^gnuplot\b/i ) {
    print("Plotting Package: Gnuplot\n");
  } elsif( $plot_package =~ /^matlab\b/i ) {
    print("Plotting Package: MatLab\n");
  } elsif( $plot_package =~ /^plotmtv\b/i ) {
    print("Plotting Package: plotmtv\n");
  } elsif( $plot_package =~ /^vtk\b/i ) {
    print("Plotting Package: VTK\n");
  } elsif( $plot_package =~ /^scidavis\b/i ) {
    print("Plotting Package: SciDAVis\n");
  } else {
    die "Error: Unrecognized Plotting Package: $plot_package.\n\n";
  }
#
#  Search for plotting package file name (output file) as second argument or prompt user.
#
  if( $ARGV[1] ) {
    $out_file = $ARGV[1];
    chomp( $out_file );
    open( OUT,">$out_file") || die "Error: Unable to Create Plotting Package File: $out_file.\n";
    print( "Created Plotting Package File: $out_file.\n");
#
#  Search for plot file name(s) as third(+) argument or prompt user.
#
    if( $ARGV[2] ) {
      print( "Plot File(s): \n" );
#
#  Check for multiple plot files with the Matlab, Surfer, VTK or SciDAVis plotting package
#
      if( $plot_package =~ /^matlab\b/i && $#ARGV > 2 ) {
        die "Error: Multiple Plot Files Specified for the Matlab Plotting Package.\n";
      }
      if( $plot_package =~ /^surfer\b/i && $#ARGV > 2 ) {
        die "Error: Multiple Plot Files Specified for the Surfer Plotting Package.\n";
      }
      if( $plot_package =~ /^vtk\b/i && $#ARGV > 2 ) {
        die "Error: Multiple Plot Files Specified for the VTK Plotting Package.\n";
      }
      if( $plot_package =~ /^scidavis\b/i && $#ARGV > 2 ) {
        die "Error: Multiple Plot Files Specified for the SciDAVis Plotting Package.\n";
      }
      for( $nf = 2; $nf <= $#ARGV; $nf++ ) {
        $plot_file[$nf-2] = $ARGV[$nf];
        $nff = $nf-1;
        print( "STOMP Plot File #$nff = $plot_file[$nf-2]\n");
      }
#
#  No third(+) argument(s); ask user for plot file name(s).
#
    } else {
      do {
        $stops = 1;
        print("STOMP Plot File Name(s)?\n");
        $names = <STDIN>;
        chomp( $names );
        @plot_file = split(/\s+/,$names);
        $names = join(",",@plot_file);
        @plot_file = split(/:+/,$names);
        $names = join(",",@plot_file);
        @plot_file = split(/,+/,$names);
#
#  Check for multiple plot files with the Matlab, Surfer, VTK or SciDAVis plotting packages
#
        if( $plot_package =~ /^matlab\b/i && $#plot_file > 0 ) {
          die "Error: Multiple Plot Files Specified for the Matlab Plotting Package.\n";
        }
        if( $plot_package =~ /^surfer\b/i && $#plot_file > 0 ) {
          die "Error: Multiple Plot Files Specified for the Surfer Plotting Package.\n";
        }
        if( $plot_package =~ /^vtk\b/i && $#plot_file > 0 ) {
          die "Error: Multiple Plot Files Specified for the VTK Plotting Package.\n";
        }
        if( $plot_package =~ /^scidavis\b/i && $#plot_file > 0 ) {
          die "Error: Multiple Plot Files Specified for the SciDAVis Plotting Package.\n";
        }
        print( "STOMP Plot File(s): \n" );
        for( $nf = 0; $nf <= $#plot_file; $nf++ ) {
          $nff = $nf+1;
          if( -r $plot_file[$nf] ) {
            print("STOMP Plot File #$nff = $plot_file[$nf]\n");
          } elsif( -B $plot_file[$nf] )  {
            $stops = 0;
            print("Error: STOMP Plot File is Binary: $plot_file[$nf].\n");
          } else {
            $stops = 0;
            print("Error: STOMP Plot File is Unreadable: $plot_file[$nf].\n");
          }
        }
        if( $stops == 0 ) { print("Try again!\n\n"); }
      } until $stops != 0;
    }
#
#  No second argument; ask user for plotting package file name (output file).
#
  } else {
    $stops = 0;
    do {
      if( $plot_package =~ /^tecplot\b/i ) {
        print("Tecplot File to Generate?\n");
      } elsif( $plot_package =~ /^tecplot10\b/i ) {
        print("Tecplot10 File to Generate?\n");
      } elsif( $plot_package =~ /^tecplotxy\b/i ) {
        print("TecplotXY File to Generate?\n");
      } elsif( $plot_package =~ /^surfer\b/i ) {
        print("Surfer File to Generate?\n");
      } elsif( $plot_package =~ /^gnuplot\b/i ) {
        print("Gnuplot File to Generate?\n");
      } elsif( $plot_package =~ /^igor\b/i ) {
        print("MatLab File to Generate?\n");
      } elsif( $plot_package =~ /^plotmtv\b/i ) {
        print("Plotmtv File to Generate?\n");
      } elsif( $plot_package =~ /^vtk\b/i ) {
        print("VTK File to Generate\n");
      } elsif( $plot_package =~ /^scidavis\b/i ) {
        print("SciDAVis File to Generate?\n");
      }
      $out_file = <STDIN>;
      chomp( $out_file );
      if( open( OUT,">$out_file") ) {
        $stops = 1;
        print("PLotting Package File: $out_file\n");
      } else {
        print("Error: Unable to Create Plotting Package File: $out_file. Try again!\n\n");
      }
    } until $stops != 0;
#
#  No second or third(+) argument(s); ask user for plot file name(s).
#
    do {
      $stops = 1;
      print("STOMP Plot File Name(s)?\n");
      $names = <STDIN>;
      chomp( $names );
      @plot_file = split(/\s+/,$names);
      $names = join(",",@plot_file);
      @plot_file = split(/:+/,$names);
      $names = join(",",@plot_file);
      @plot_file = split(/,+/,$names);
#
#  Check for multiple plot files with the Matlab, Surfer, VTK or SciDAVis plotting package
#
      if( $plot_package =~ /^matlab\b/i && $#plot_file > 0 ) {
        die "Error: Multiple Plot Files Specified for the Matlab Plotting Package.\n";
      }
      if( $plot_package =~ /^surfer\b/i && $#plot_file > 0 ) {
        die "Error: Multiple Plot Files Specified for the Surfer Plotting Package.\n";
      }
      if( $plot_package =~ /^vtk\b/i && $#plot_file > 0 ) {
        die "Error: Multiple Plot Files Specified for the VTK Plotting Package.\n";
      }
      if( $plot_package =~ /^scidavis\b/i && $#plot_file > 0 ) {
        die "Error: Multiple Plot Files Specified for the SciDAVis Plotting Package.\n";
      }
      print( "Plot File(s): \n" );
      for( $nf = 0; $nf <= $#plot_file; $nf++ ) {
        $nff = $nf+1;
        if( -r $plot_file[$nf] ) {
          print("Plot File #$nff = $plot_file[$nf]\n");
        } elsif( -B $plot_file[$nf] )  {
          $stops = 0;
          print("Error: Plot File is Binary: $plot_file[$nf].\n");
        } else {
          $stops = 0;
          print("Error: Plot File is Unreadable: $plot_file[$nf].\n");
        }
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
    print("Plotting Package [Gnuplot|MatLab|Plotmtv|Surfer|Tecplot|TecplotXY|Tecplot10|VTK|SciDAVis]?\n");
    $plot_package = <STDIN>;
    chomp( $plot_package );
    if( $plot_package =~ /^tecplot\b/i ) {
      print("Plotting Package: Tecplot\n");
      $stops = 1;
    } elsif( $plot_package =~ /^tecplot10\b/i ) {
      print("Plotting Package: Tecplot10\n");
      $stops = 1;
    } elsif( $plot_package =~ /^tecplotxy\b/i ) {
      print("Plotting Package: TecplotXY\n");
      $stops = 1;
    } elsif( $plot_package =~ /^gnuplot\b/i ) {
      print("Plotting Package: Gnuplot\n");
      $stops = 1;
    } elsif( $plot_package =~ /^surfer\b/i ) {
      print("Plotting Package: Surfer\n");
      $stops = 1;
    } elsif( $plot_package =~ /^matlab\b/i ) {
      print("Plotting Package: MatLab\n");
      $stops = 1;
    } elsif( $plot_package =~ /^plotmtv\b/i ) {
      print("Plotting Package: plotmtv\n");
      $stops = 1;
    } elsif( $plot_package =~ /^vtk\b/i ) {
      print("Plotting Package: VTK\n");
      $stops = 1;
    } elsif( $plot_package =~ /^scidavis\b/i ) {
      print("Plotting Package: SciDAVis\n");
      $stops = 1;
    } else {
      print("Error: Unrecognized Plotting Package: $plot_package.  Try again!\n\n");
    }
  } until $stops != 0;
#
#  No first or second argument; ask user for output file name.
#
  $stops = 0;
  do {
      if( $plot_package =~ /^tecplot\b/i ) {
        print("Tecplot File to Generate?\n");
      } elsif( $plot_package =~ /^tecplot10\b/i ) {
        print("Tecplot10 File to Generate?\n");
      } elsif( $plot_package =~ /^tecplotXY\b/i ) {
        print("TecplotXY File to Generate?\n");
      } elsif( $plot_package =~ /^surfer\b/i ) {
        print("Surfer File to Generate?\n");
      } elsif( $plot_package =~ /^gnuplot\b/i ) {
        print("Gnuplot File to Generate?\n");
      } elsif( $plot_package =~ /^igor\b/i ) {
        print("MatLab File to Generate?\n");
      } elsif( $plot_package =~ /^plotmtv\b/i ) {
        print("Plotmtv File to Generate?\n");
      } elsif( $plot_package =~ /^vtk\b/i ) {
        print("VTK File to Generate\n");
      } elsif( $plot_package =~ /^scidavis\b/i ) {
        print("SciDAVis File to Generate?\n");
      }
    $out_file = <STDIN>;
    chomp( $out_file );
    if( open( OUT,">$out_file") ) {
      $stops = 1;
      print("PLotting Package File to Generate: $out_file\n");
    } else {
      print("Error: Unable to Create Plotting Package File: $out_file. Try again!\n\n");
    }
  } until $stops != 0;
#
#  No first, second or third(+) argument(s); ask user for plot file name(s).
#
  do {
    $stops = 1;
    print("STOMP Plot File Name(s)?\n");
    $names = <STDIN>;
    chomp( $names );
    @plot_file = split(/\s+/,$names);
    $names = join(",",@plot_file);
    @plot_file = split(/:+/,$names);
    $names = join(",",@plot_file);
    @plot_file = split(/,+/,$names);
#
#  Check for multiple plot files with the Matlab, Surfer, VTK or SCiDAVis plotting packages
#
    if( $plot_package =~ /^matlab\b/i && $#plot_file > 0 ) {
      die "Error: Multiple Plot Files Specified for the Matlab Plotting Package.\n";
    }
    if( $plot_package =~ /^surfer\b/i && $#plot_file > 0 ) {
      die "Error: Multiple Plot Files Specified for the Surfer Plotting Package.\n";
    }
    if( $plot_package =~ /^vtk\b/i && $#plot_file > 0 ) {
      die "Error: Multiple Plot Files Specified for the VTK Plotting Package.\n";
    }
    if( $plot_package =~ /^scidavis\b/i && $#plot_file > 0 ) {
      die "Error: Multiple Plot Files Specified for the SciDAVis Plotting Package.\n";
    }
    print( "Plot File(s): \n" );
    for( $nf = 0; $nf <= $#plot_file; $nf++ ) {
      $nff = $nf+1;
      if( -r $plot_file[$nf] ) {
        print("STOMP Plot File #$nff = $plot_file[$nf]\n");
      } elsif( -B $plot_file[$nf] )  {
        $stops = 0;
        print("Error: STOMP Plot File is Binary: $plot_file[$nf].\n");
      } else {
        $stops = 0;
        print("Error: STOMP Plot File is Unreadable: $plot_file[$nf].\n");
      }
    }
    if( $stops == 0 ) { print("Try again!\n\n"); }
  } until $stops != 0;
}
#
#  Gnuplot Grid Resolution Factor
#
  if( $plot_package =~ /^gnuplot\b/i && $res_opt == 1 ) {
    print("Gnuplot Grid Resolution Factor?\n");
    $res_fac = <STDIN>;
    chomp( $res_fac );
  }
#
#  Gnuplot Title
#
  if( $plot_package =~ /^gnuplot\b/i && $t_opt == 1 ) {
    print("Gnuplot Title?\n");
    $title = <STDIN>;
    chomp( $title );
  }
#
#  Tecplot Title
#
  if( ($plot_package =~ /^tecplot\b/i || $plot_package =~ /^tecplot10\b/i ||
     $plot_package =~ /^tecplotxy\b/i ) 
    && $t_opt == 1 ) {
    print("Tecplot Title?\n");
    $title = <STDIN>;
    chomp( $title );
  }
#
#  Tecplot Time Stamp
#
  if( ($plot_package =~ /^tecplot\b/i || $plot_package =~ /^gnuplot\b/i || 
    $plot_package =~ /^plotmtv\b/i || $plot_package =~ /^tecplot10\b/i  || 
    $plot_package =~ /^tecplotxy\b/i)
    && $ts_opt == 1 ) {
    $stops = 0;
    do {
      print("Tecplot or Gnuplot Time Stamp Unit?\n");
      print("Options: s[econd] | m[inute] | h[our] | d[ay] | w[eek] | y[ear]\n");
      $tsunit = <STDIN>;
      chomp( $tsunit );
      if( $tsunit =~ /^s/i ) {
        $tsunit = "s";
        $stops = 1;
      } elsif( $tsunit =~ /^m/i ) {
        $tsunit = "min";
        $stops = 1;
      } elsif( $tsunit =~ /^h/i ) {
        $tsunit = "h";
        $stops = 1;
      } elsif( $tsunit =~ /^d/i ) {
        $tsunit = "day";
        $stops = 1;
      } elsif( $tsunit =~ /^w/i ) {
        $tsunit = "wk";
        $stops = 1;
      } elsif( $tsunit =~ /^y/i ) {
        $tsunit = "yr";
        $stops = 1;
      } else {
        print("Error: Unrecognized Time Stamp Unit: $tsunit.  Try again!\n\n");
      }
    } until $stops != 0;
  }
#
#  Tecplot Time Adjustment
#
  if( ($plot_package =~ /^tecplot\b/i || $plot_package =~ /^gnuplot\b/i || 
    $plot_package =~ /^plotmtv\b/i || $plot_package =~ /^tecplot10\b/i ||
    $plot_package =~ /^tecplotxy\b/i )
    && $ta_opt == 1 ) {
    print("Time Adjustment?\n");
    $tadj = <STDIN>;
    chomp( $tadj );
  }
#
#  Loop over plot files
#
for( $nf = 0; $nf <= $#plot_file; $nf++ ) {
  open( PLOT,$plot_file[$nf] ) || die "Error: Unable to Open STOMP Plot File: $plot_file[$nf].\n";
  print("Converting STOMP Plot File: $plot_file[$nf].\n");
  @plot_array = <PLOT>;
#
#  Initialize flags
#
  $xflag = 0;
  $yflag = 0;
  $zflag = 0;
  $xvflag = 0;
  $yvflag = 0;
  $zvflag = 0;
  $nvflag = 0;
  $ixpflag = 0;
  $fvflag = -1;
  $nfv = 0;
  $xorig = 0;
  $yorig = 0;
  $zorig = 0;
  $nvert = 1;
  $nbrn = 0;
  $nfnc = 0;
#
#  Loop over lines in plot file
#
  foreach $plot_line (@plot_array) {
#
#  Remove return from line
#
    chomp( $plot_line );
#
#  Remove leading blank spaces from line
#
    $plot_line =~ s/^\s+//;
#
#  Set the number of x-direction nodes
#
    if( $plot_line =~ /Number of X or R-Direction Nodes/ ) {
      @fields = split(/\s+/,$plot_line);
      $ifld = $fields[$#fields];
#
#  Set the number of y-direction nodes
#
    } elsif( $plot_line =~ /Number of Y or Theta-Direction Nodes/ ) {
      @fields = split(/\s+/,$plot_line);
      $jfld = $fields[$#fields];
#
#  Set the number of z-direction nodes
#
    } elsif( $plot_line =~ /Number of Z-Direction Nodes/ ) {
      @fields = split(/\s+/,$plot_line);
      $kfld = $fields[$#fields];
      $nfld = $ifld*$jfld*$kfld;
      $nsx = ($ifld+1)*$jfld*$kfld;
      $nsy = $ifld*($jfld+1)*$kfld;
      $nsz = $ifld*$jfld*($kfld+1);
      $ifldx = $ifld+1;
      $jfldx = $jfld+1;
      $kfldx = $kfld+1;
#
#  Set the number of block refinement nodes
#
    } elsif( $plot_line =~ /Number of Block Refinement Nodes/ ) {
      @fields = split(/\s+/,$plot_line);
      $nbrn = $fields[$#fields];
#
#  Set the number of inactive nodes
#
    } elsif( $plot_line =~ /Number of Active Nodes/ ) {
      @fields = split(/\s+/,$plot_line);
      $nactn = $fields[$#fields];
#
#  Set the number of vertices
#
    } elsif( $plot_line =~ /Number of Vertices/ ) {
      @fields = split(/\s+/,$plot_line);
      $nvert = $fields[$#fields];
#
#  Set the x origin
#
    } elsif( $plot_line =~ /X Origin/ ) {
#
#     Surface positions recorded in plot file
#
      if( $plot_line =~ /X Origin -- Surface Positions/ ) {
        $xsurf = 1;
      } elsif( $plot_line =~ /X Origin -- Hexahedra Points/ ) {
        $xhexa = 1;
      } else {
        @fields = split(/\s+/,$plot_line);
        $xorig = $fields[$#fields];
      }
#
#  Set the y origin
#
    } elsif( $plot_line =~ /Y Origin/ ) {
#
#     Surface positions recorded in plot file
#
      if( $plot_line =~ /Y Origin -- Surface Positions/ ) {
        $ysurf = 1;
      } elsif( $plot_line =~ /Y Origin -- Hexahedra Points/ ) {
        $yhexa = 1;
      } else {
        @fields = split(/\s+/,$plot_line);
        $yorig = $fields[$#fields];
      }
#
#  Set the z origin
#
    } elsif( $plot_line =~ /Z Origin/ ) {
#
#     Surface positions recorded in plot file
#
      if( $plot_line =~ /Z Origin -- Surface Positions/ ) {
        $zsurf = 1;
      } elsif( $plot_line =~ /Z Origin -- Hexahedra Points/ ) {
        $zhexa = 1;
      } else {
        @fields = split(/\s+/,$plot_line);
        $zorig = $fields[$#fields];
      }
#
#  Time stamp
#
    } elsif( $plot_line =~ /Time = / && $ts_opt == 1 ) {
      @fields = split(/\s+|,/,$plot_line);
      for( $i = 3; $i <= $#fields; $i++ ) {
        if( $fields[$i] =~ /$tsunit/ ) {
          push(@tsv,$fields[$i-1]);
        }
      }
#
#  Located a character string starting a line
#
    } elsif( $plot_line =~ /^[A-Z]|^[a-z]/ ) {
#
#  Set flag to read x-direction node positions
#
      if( $plot_line =~ /X or R-Direction Node Positions/ ||
        $plot_line =~ /Radial-Direction Node Positions/ ||
        $plot_line =~ /X-Direction Surface Positions/ ||
        $plot_line =~ /X-Direction Node Positions/ ) {
        @fields = split(/,/,$plot_line);
        $xunit = $fields[1];
        $xunit =~ s/\s+//g;
        $xflag = 1;
#
#  Set flag to read y-direction node positions
#
      } elsif( $plot_line =~ /Y or Theta-Direction Node Positions/ ||
        $plot_line =~ /Theta-Direction Node Positions/ ||
        $plot_line =~ /Y-Direction Surface Positions/ ||
        $plot_line =~ /Y-Direction Node Positions/ ) {
        @fields = split(/,/,$plot_line);
        $yunit = $fields[1];
        $yunit =~ s/\s+//g;
        $yflag = 1;
#
#  Set flag to read z-direction node positions
#
      } elsif( $plot_line =~ /Z-Direction Node Positions/ ||
        $plot_line =~ /Z-Direction Surface Positions/ ) {
        @fields = split(/,/,$plot_line);
        $zunit = $fields[1];
        $zunit =~ s/\s+//g;
        $zflag = 1;
#
#  Set flag to read x-direction node positions
#
      } elsif( $plot_line =~ /X-Direction Nodal Vertices/ ) {
        @fields = split(/,/,$plot_line);
        $xunit = $fields[1];
        $xunit =~ s/\s+//g;
        $xvert = 1;
        $xflag = 1;
#
#  Set flag to read y-direction node positions
#
      } elsif( $plot_line =~ /Y-Direction Nodal Vertices/ ) {
        @fields = split(/,/,$plot_line);
        $yunit = $fields[1];
        $yunit =~ s/\s+//g;
        $yvert = 1;
        $yflag = 1;
#
#  Set flag to read z-direction node positions
#
      } elsif( $plot_line =~ /Z-Direction Nodal Vertices/ ) {
        @fields = split(/,/,$plot_line);
        $zunit = $fields[1];
        $zunit =~ s/\s+//g;
        $zvert = 1;
        $zflag = 1;
#
#  Set flag to read node volumes
#
      } elsif( $plot_line =~ /Node Volume/ ) {
        @fields = split(/,/,$plot_line);
        $nvunit = $fields[1];
        $nvunit =~ s/\s+//g;
        $nvflag = 1;
#
#  Set flag to read inactive nodes
#
      } elsif( $plot_line =~ /Inactive Nodes/ || 
        $plot_line =~ /Node Map/ ) {
        $ixpflag = 1;
#
#  Set flag to read node-centered flux variables
#
      } elsif( $plot_line =~ /Node Centered/ ) {
        @fields = split(/,/,$plot_line);
        push(@fvname,$fields[0]);
        $unit = $fields[1];
        $unit =~ s/\s+//g;
        push(@fvunit,$unit);
        $fvflag = 1;
        $nfv++;
#
#  Set flag to read intrinsic permeability variables
#
      } elsif( $plot_line =~ /Intrinsic Perm/ ) {
        @fields = split(/,/,$plot_line);
        push(@fvname,$fields[0]);
        $unit = $fields[1];
        $unit =~ s/\s+//g;
        push(@fvunit,$unit);
        $fvflag = 1;
        $nfv++;
#
#  Set flag to read x-direction flux variables
#
      } elsif( $plot_line =~ /X-Dir./ ) {
        @fields = split(/,/,$plot_line);
        $xvunit = $fields[1];
        $xvunit =~ s/\s+//g;
        $xvflag = 1;
#
#  Set flag to read y-direction flux variables
#
      } elsif( $plot_line =~ /Y-Dir./ ) {
        @fields = split(/,/,$plot_line);
        $yvunit = $fields[1];
        $yvunit =~ s/\s+//g;
        $yvflag = 1;
#
#  Set flag to read z-direction flux variables
#
      } elsif( $plot_line =~ /Z-Dir./ ) {
        @fields = split(/,/,$plot_line);
        $zvunit = $fields[1];
        $zvunit =~ s/\s+//g;
        $zvflag = 1;
#
#  Set flag to read field variables
#
      } elsif( $fvflag >= 0 ) {
        @fields = split(/,/,$plot_line);
        push(@fvname,$fields[0]);
        $unit = $fields[1];
        $unit =~ s/\s+//g;
        push(@fvunit,$unit);
        $fvflag = 1;
        $nfv++;
      }
#
#  Read x-direction node positions
#
    } elsif( $xflag != 0 ) {
      if( $xsurf ) {
        @fields = split(/\s+/,$plot_line);
        push(@xp,@fields);
        if( ($#xp+1) >= $ifldx*$jfldx*$kfldx ){ $xflag = 0; }
      } elsif( $xhexa ) {
        @fields = split(/\s+/,$plot_line);
        push(@xp,@fields);
        if( ($#xp+1) >= $ifldx*$jfldx*$kfldx ){ $xflag = 0; }
      } elsif( $xvert ) {
        @fields = split(/\s+/,$plot_line);
        push(@xp,@fields);
        $nsum = $nfld + $nbrn;
        if( ($#xp+1) >= $nvert*$nsum ){ $xflag = 0; }
      } else {
        @fields = split(/\s+/,$plot_line);
        push(@xp,@fields);
        if( ($#xp+1) >= $nfld ){ $xflag = 0; }
      }
#
#  Read y-direction node positions
#
    } elsif( $yflag != 0 ) {
      if( $ysurf ) {
        @fields = split(/\s+/,$plot_line);
        push(@yp,@fields);
        if( ($#yp+1) >= $ifldx*$jfldx*$kfldx ){ $yflag = 0; }
      } elsif( $yhexa ) {
        @fields = split(/\s+/,$plot_line);
        push(@yp,@fields);
        if( ($#yp+1) >= $ifldx*$jfldx*$kfldx ){ $yflag = 0; }
      } elsif( $yvert ) {
        @fields = split(/\s+/,$plot_line);
        push(@yp,@fields);
        $nsum = $nfld + $nbrn;
        if( ($#yp+1) >= $nvert*$nsum ){ $yflag = 0; }
      } else {
        @fields = split(/\s+/,$plot_line);
        push(@yp,@fields);
        if( ($#yp+1) >= $nfld ) { $yflag = 0; }
      }
#
#  Read z-direction node positions
#
    } elsif( $zflag != 0 ) {
      if( $zsurf ) {
        @fields = split(/\s+/,$plot_line);
        push(@zp,@fields);
        if( ($#zp+1) >= $ifldx*$jfldx*$kfldx ){ $zflag = 0; }
      } elsif( $zhexa ) {
        @fields = split(/\s+/,$plot_line);
        push(@zp,@fields);
        if( ($#zp+1) >= $ifldx*$jfldx*$kfldx ){ $zflag = 0; }
      } elsif( $zvert ) {
        @fields = split(/\s+/,$plot_line);
        push(@zp,@fields);
        $nsum = $nfld + $nbrn;
        if( ($#zp+1) >= $nvert*$nsum ){ $zflag = 0; }
      } else {
        @fields = split(/\s+/,$plot_line);
        push(@zp,@fields);
        if( ($#zp+1) >= $nfld ) { $zflag = 0; }
      }
#
#  Read node volumes
#
    } elsif( $nvflag != 0 ) {
      @fields = split(/\s+/,$plot_line);
      push(@nv,@fields);
      $nsum = $nfld+$nbrn;
      if( ($#nv+1) >= $nsum ) {
        $nvflag = 0;
        $fvflag = 0;
      }
#
#  Read inactive nodes
#
    } elsif( $ixpflag != 0 ) {
      @fields = split(/\s+/,$plot_line);
      push(@ixp,@fields);
      $nsum = $nfld+$nbrn;
      if( ($#ixp+1) >= $nsum ) {
        $ixpflag = 0;
        $fvflag = 0;
      }
#
#  Read x-direction variables
#
    } elsif( $xvflag != 0 ) {
      @fields = split(/\s+/,$plot_line);
      push(@xv,@fields);
      if( ($#xv+1) >= $nsx ) {
        $xvflag = 0;
        $fvflag = 0;
      }
#
#  Read y-direction variables
#
    } elsif( $yvflag != 0 ) {
      @fields = split(/\s+/,$plot_line);
      push(@yv,@fields);
      if( ($#yv+1) >= $nsy ) {
        $yvflag = 0;
        $fvflag = 0;
      }
#
#  Read z-direction variables
#
    } elsif( $zvflag != 0 ) {
      @fields = split(/\s+/,$plot_line);
      push(@zv,@fields);
      if( ($#zv+1) >= $nsz ) {
        $zvflag = 0;
        $fvflag = 0;
      }
#
#  Read field variable
#
    } elsif( $fvflag > 0 ) {
      @fields = split(/\s+/,$plot_line);
      push(@fv,@fields);
      $nsum = $nfld+$nbrn;
      if( ($#fv+1) >= ($nsum*$nfv) ) { $fvflag = 0; }
    }
  }
#
#  Write plotmtv output file
#
  if( $plot_package =~ /^plotmtv\b/i ) {
#
#   Prompt user for plot-file variables
#
    if( $rdfv_opt == 0 ) {
      $stops = 0;
      W2: while( $stops == 0 ) {
        print "The STOMP plot file, \"$plot_file[0]\", contains the\n";
        print "following field variables:\n\n";
        for( $i = 0; $i <= $#fvname; $i++ ) {
          print "$i --";
          if( $fvunit[$i] ) {
            print " \"$fvname[$i], $fvunit[$i]\"\n";
          } else {
            print " \"$fvname[$i]\"\n";
          }
        }
        print "\nEnter the plot-field variable to include in the\n";
        print "plotting-package input file, by entering a single index...\n";
        $in_line = <STDIN>;
        chomp( $in_line );
        $in_line =~ s/^\s+//;
        @entries = split(/\s+|,/,$in_line);
        if( $#entries > 1 ) {
          print("\nError: Multiple Entries\n");
          print("Try again!\n\n");
          redo W2
        }
        $ifv = $entries[0];
        if( !($ifv =~ /[0-9]+/i) )
        {
          print ("\nError: Unrecognized Index: $ifv.\n\n");
          print("Try again!\n\n");
          redo W2;
        }
        if( $ifv <0 || $ifv > $#fvname ) {
          print("\nError: Unrecognized Index: $ifv.\n\n");
          print("Try again!\n\n");
          redo W2;
        }
        $stops = 1;
      }
      $rdfv_opt = 1;
    }
#
#   Write three-dimensional contour files
#
    if( $ifld > 1 && $jfld > 1 && $kfld > 1 ) {
      print OUT ("\$ DATA=GRID4D\n\n");
      if( $ts_opt == 1 ) {
        print OUT ("% toplabel = \"$tsv[$nf], $tsunit\"\n");
      } else {
      print OUT ("% toplabel = \"$plot_file[$nf]\"\n");
      }
      print OUT ("% subtitle = \"$fvname[$ifv], $fvunit[$ifv]\"\n");
      print OUT ("% contstyle = 2\n\n");
#
#     Write x grid data
#
      $nc = 0;
      print OUT ("% NX = $ifld XGRID=TRUE\n");
      for( $i = 0; $i < $ifld; $i++ ) {
          $np = $i;
        $xs = 0.0;
        for( $iv = 0; $iv < $nvert; $iv++ ) {
          $ix = $i*$nvert + $iv;
          $xs += $xp[$ix];
        }
        $xs /= $nvert;
        print OUT "$xs ";
        $nc++;
        if( $nc == 10 ) {
          print OUT "\n";
          $nc = 0;
        }
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
      print OUT "\n";
#  
#     Write y grid data
#  
      $nc = 0;
      print OUT ("% NY = $jfld YGRID=TRUE\n");
      for( $j = 0; $j < $jfld; $j++ ) {
          $np = $j*$ifld;
        $ys = 0.0;
        for( $iv = 0; $iv < $nvert; $iv++ ) {
          $ix = $np*$nvert + $iv;
          $ys += $yp[$ix];
        }
        $ys /= $nvert;
        print OUT "$ys ";
        $nc++;
        if( $nc == 10 ) {
          print OUT "\n";
          $nc = 0;
        }
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
      print OUT "\n";
#
#     Write z grid data
#
      $nc = 0;
      print OUT ("% NZ = $kfld ZGRID=TRUE\n");
      for( $k = 0; $k < $kfld; $k++ ) {
          $np = $k*$ifld*$jfld;
        $zs = 0.0;
        for( $iv = 0; $iv < $nvert; $iv++ ) {
          $ix = $np*$nvert + $iv;
          $zs += $zp[$ix];
        }
        $zs /= $nvert;
        print OUT "$zs ";
        $nc++;
        if( $nc == 10 ) {
          print OUT "\n";
          $nc = 0;
        }
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
      print OUT "\n";
#
#     Write plot control structures
#
      print OUT ("#% vxmin = 0 vxmax = 0\n");
      print OUT ("#% vymin = 0 vymax = 0\n");
      print OUT ("#% vzmin = 0 vzmax = 0\n");
      print OUT ("#% cmin = 0 cmax = 0 cstep = 0\n"); 
      print OUT ("% equalscale = false\n");
      print OUT ("% fitpage = true\n\n");
#
#
#     Write field-plot data
#
      $nc = 0;
      for( $i = 0; $i < $nfld; $i++ ) {
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j*$nfld + $i;
        if( $j == $ifv ) {
          print OUT " $fv[$k]";
          $nc++;
          if( $nc == 10 ) {
            print OUT "\n";
            $nc = 0;
          }
        }
      }
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
      if( $nf == $#plot_file ) {
        print OUT ("\$ END\n\n");
      } else {
        print OUT "\n";
      }
#
#   Write two-dimensional contour files
#
    } elsif( ($ifld > 1 && $jfld > 1) || ($jfld > 1 && $kfld > 1) || ($kfld > 1 && $ifld > 1)  ) {
      print OUT ("\$ DATA=CONTOUR\n\n");
      if( $ts_opt == 1 ) {
        print OUT ("% toplabel = \"$tsv[$nf], $tsunit\"\n");
      } else {
      print OUT ("% toplabel = \"$plot_file[$nf]\"\n");
      }
      print OUT ("% subtitle = \"$fvname[$ifv], $fvunit[$ifv]\"\n");
      print OUT ("% contstyle = 2\n\n");
#
#     Write x grid data
#
      $nc = 0;
      if( $ifld>1 ) {
        print OUT ("% NX = $ifld XGRID=TRUE\n");
        $nx = $ifld;
        for( $i = 0; $i < $ifld; $i++ ) {
            $np = $i;
          $xs = 0.0;
          for( $iv = 0; $iv < $nvert; $iv++ ) {
            $ix = $np*$nvert + $iv;
            $xs += $xp[$ix];
          }
          $xs /= $nvert;
          print OUT "$xs ";
          $nc++;
          if( $nc == 10 ) {
            print OUT "\n";
            $nc = 0;
          }
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
        print OUT "\n";
      }
#  
#     Write y grid data
#  
      $nc = 0;
      if( $jfld>1 ) {
        if( $ifld<=1 ) {
          print OUT ("% NX = $jfld XGRID=TRUE\n");
          $nx = $jfld;
        } else {
          print OUT ("% NY = $jfld YGRID=TRUE\n");
        }
        for( $j = 0; $j < $jfld; $j++ ) {
            $np = $j*$ifld;
          $ys = 0.0;
          for( $iv = 0; $iv < $nvert; $iv++ ) {
            $ix = $np*$nvert + $iv;
            $ys += $yp[$ix];
          }
          $ys /= $nvert;
          print OUT "$ys ";
          $nc++;
          if( $nc == 10 ) {
            print OUT "\n";
            $nc = 0;
          }
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
        print OUT "\n";
      }
#
#     Write z grid data
#
      $nc = 0;
      if( $kfld>1 ) {
        print OUT ("% NY = $kfld YGRID=TRUE\n");
        for( $k = 0; $k < $kfld; $k++ ) {
            $np = $k*$ifld*$jfld;
          $zs = 0.0;
          for( $iv = 0; $iv < $nvert; $iv++ ) {
            $ix = $np*$nvert + $iv;
            $zs += $zp[$ix];
          }
          $zs /= $nvert;
          print OUT "$zs ";
          $nc++;
          if( $nc == 10 ) {
            print OUT "\n";
            $nc = 0;
          }
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
        print OUT "\n";
      }
#
      print OUT ("#% vxmin =  vxmax =  \n");
      print OUT ("#% vymin =  vymax =  \n");
      print OUT ("#% cmin = 40 cmax = 60 cstep = 1\n"); 
      print OUT ("% equalscale = false\n");
      print OUT ("% fitpage = true\n");
#      print OUT ("% nsteps = 20\n");  
      
#
#
#     Write field-plot data
#
      $nc = 0;
      for( $i = 0; $i < $nfld; $i++ ) {
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j*$nfld + $i;
        if( $j == $ifv ) {
          print OUT " $fv[$k]";
          $nc++;
#          if( $nc == $nx ) {
          if( $nc == 10 ) {
            print OUT "\n";
            $nc = 0;
          }
        }
      }
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
      if( $nf == $#plot_file ) {
        print OUT ("\$ END\n\n");
      } else {
        print OUT "\n";
      }
#
#   Write line plot files
#
    } else {
      if( $nf == 0 ) {
        print OUT ("\$ DATA=CURVE2D\n");
        print OUT ("% toplabel = \"$fvname[$ifv], $fvunit[$ifv]\"\n");
      }
      if( $flipxy_opt == 1 ) {
        if( $ifld>1 ) {
          print OUT ("% ylabel =  \"Distance, $xunit\"\n");
        }elsif( $jfld>1 ) {  
          print OUT ("% ylabel =  \"Distance, $yunit\"\n");
        }elsif( $kfld>1 ) {  
          print OUT ("% ylabel =  \"Distance, $zunit\"\n");
        }
        print OUT ("% xlabel = \"$fvname[$ifv], $fvunit[$ifv]\"\n");
      } else {
        if( $ifld>1 ) {
          print OUT ("% xlabel =  \"Distance, $xunit\"\n");
        }elsif( $jfld>1 ) {  
          print OUT ("% xlabel =  \"Distance, $yunit\"\n");
        }elsif( $kfld>1 ) {  
          print OUT ("% xlabel =  \"Distance, $zunit\"\n");
        }
        print OUT ("% ylabel = \"$fvname[$ifv], $fvunit[$ifv]\"\n");
      }
      print OUT ("% xmin = \n");
      print OUT ("% xmax = \n");
      print OUT ("% ymin = \n");
      print OUT ("% ymax = \n");
      print OUT ("% equalscale = false\n");
      print OUT ("% fitpage = true\n");

#      
      $nfx = $nf + 1;
#      
#      print OUT ("% linetype = $nfx\n");
      print OUT ("% linecolor = $nfx\n");
#      print OUT ("% markertype = $nfx\n");
      if( $ts_opt == 1 ) {
        print OUT ("% linelabel = \"$tsv[$nf], $tsunit\"\n");
      } else {
      print OUT ("% linelabel = \"$plot_file[$nf]\"\n");
      }
#
#     Write x grid data
#
      if( $ifld>1 ) {
        for( $i = 0; $i < $ifld; $i++ ) {
            $np = $i;
          $xs = 0.0;
          for( $iv = 0; $iv < $nvert; $iv++ ) {
            $ix = $np*$nvert + $iv;
            $xs += $xp[$ix];
          }
          $xs /= $nvert;
          $ix = $ifv*$nfld + $i;
          if( $flipxy_opt == 1 ) {
            print OUT "$fv[$ix] $xs\n";
          } else { 
            print OUT "$xs $fv[$ix]\n";
          }
        }
        print OUT "\n";
      }
#  
#     Write y grid data
#  
      $nc = 0;
      if( $jfld>1 ) {
        for( $j = 0; $j < $jfld; $j++ ) {
            $np = $j*$ifld;
          $ys = 0.0;
          for( $iv = 0; $iv < $nvert; $iv++ ) {
            $ix = $np*$nvert + $iv;
            $ys += $yp[$ix];
          }
          $ys /= $nvert;
          $jx = $ifv*$nfld + $j;
          if( $flipxy_opt == 1 ) {
            print OUT "$fv[$jx] $ys\n";
          } else { 
            print OUT "$ys $fv[$jx]\n";
          }
        }
        print OUT "\n";
      }
#
#     Write z grid data
#
      $nc = 0;
      if( $kfld>1 ) {
        for( $k = 0; $k < $kfld; $k++ ) {
            $np = $k*$ifld*$jfld;
          $zs = 0.0;
          for( $iv = 0; $iv < $nvert; $iv++ ) {
            $ix = $np*$nvert + $iv;
            $zs += $zp[$ix];
          }
          $zs /= $nvert;
          $kx = $ifv*$nfld + $k;
          if( $flipxy_opt == 1 ) {
            print OUT "$fv[$kx] $zs\n";
          } else { 
            print OUT "$zs $fv[$kx]\n";
          }
        }
        print OUT "\n";
      }
    }
  }
  
#
#  Write Surfer output file
#
  if( $plot_package =~ /^surfer\b/i ) {
#
#  X-direction vertices only
#
    if( $xvert == 1 && $ifld > 1 && $jfld == 1 && $kfld == 1 ) {
      print OUT "\"X, $xunit\" \"Volume, $nvunit\"";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT " \"$fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT " \"$fvname[$i]\"";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $xs = ($xp[$j1]+$xp[$j2])/2.;
        print OUT "$xs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  Y-direction vertices only
#
    } elsif( $yvert == 1 && $jfld > 1 && $ifld == 1 && $kfld == 1 ) {
      print OUT "\"Y, $yunit\" \"Volume, $nvunit\"";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT " \"$fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT " \"$fvname[$i]\"";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $ys = ($yp[$j1]+$yp[$j2])/2.;
        print OUT "$ys $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  Z-direction vertices only 
#
    } elsif( $zvert == 1 && $kfld > 1 && $ifld == 1 && $jfld == 1 ) {
      print OUT "\"Z, $zunit\" \"Volume, $nvunit\"";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT " \"$fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT " \"$fvname[$i]\"";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $zs = ($zp[$j1]+$zp[$j2])/2.;
        print OUT "$zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction and Y-direction vertices only
#
    } elsif( $xvert == 1 && $yvert == 1  && $ifld > 1 && $jfld > 1 && $kfld == 1 ) {
      print OUT "\"X, $xunit\" \"Y, $yunit\" \"Volume, $nvunit\"";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT " \"$fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT " \"$fvname[$i]\"";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/4.;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/4.;
        print OUT "$xs $ys $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction and Y-direction vertices only
#
    } elsif( $xvert == 1 && $zvert == 1  && $ifld > 1 && $kfld > 1 && $jfld == 1 ) {
      print OUT "\"X, $xunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT " \"$fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT " \"$fvname[$i]\"";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/4.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/4.;
        print OUT "$xs $zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  Y-direction and Z-direction vertices only
#
    } elsif( $yvert == 1 && $zvert == 1  && $jfld > 1 && $kfld > 1 && $ifld == 1 ) {
      print OUT "\"Y, $yunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT " \"$fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT " \"$fvname[$i]\"";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/4.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/4.;
        print OUT "$ys $zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction, Y-direction and Z-direction vertices only
#
    } else {
      print OUT "\"X, $xunit\" \"Y, $yunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT " \"$fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT " \"$fvname[$i]\"";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $j5 = $j1+4;
        $j6 = $j1+5;
        $j7 = $j1+6;
        $j8 = $j1+7;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/8.;
        $xs += ($xp[$j5]+$xp[$j6]+$xp[$j7]+$xp[$j8])/8.;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/8.;
        $ys += ($yp[$j5]+$yp[$j6]+$yp[$j7]+$yp[$j8])/8.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/8.;
        $zs += ($zp[$j5]+$zp[$j6]+$zp[$j7]+$zp[$j8])/8.;
        print OUT "$xs $ys $zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
    }
  }
#
#  Write SciDAVis output file
#
  if( $plot_package =~ /^scidavis\b/i ) {
#
#  X-direction vertices only
#
    if( $xvert == 1 && $yvert != 1  && $zvert != 1 && $ifld > 1 ) {
      print OUT "X($xunit),Volume($nvunit)";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT ",$fvname[$i]($fvunit[$i])";
        } else {
          print OUT ",$fvname[$i]";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $xs = ($xp[$j1]+$xp[$j2])/2.;
#        print OUT "$xs($nv[$i])";
        print OUT "$xs,$nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT ",$fv[$k]";
        }
        print OUT "\n";
      }
#
#  Y-direction vertices only
#
    } elsif( $yvert == 1 && $xvert != 1  && $zvert != 1 && $jfld > 1 ) {
      print OUT "Y($yunit),Volume($nvunit)";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT ",$fvname[$i]($fvunit[$i])";
        } else {
          print OUT ",$fvname[$i]";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $ys = ($yp[$j1]+$yp[$j2])/2.;
#        print OUT "$ys($nv[$i])";
        print OUT "$ys,$nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT ",$fv[$k]";
        }
        print OUT "\n";
      }
#
#  Z-direction vertices only 
#
    } elsif( $zvert == 1 && $xvert != 1  && $yvert != 1 && $kfld > 1 ) {
      print OUT "Z($zunit),Volume($nvunit)";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT ",$fvname[$i]($fvunit[$i])";
        } else {
          print OUT ",$fvname[$i]";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $zs = ($zp[$j1]+$zp[$j2])/2.;
#        print OUT "$zs($nv[$i])";
        print OUT "$zs,$nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT ",$fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction and Y-direction vertices only
#
    } elsif( $xvert == 1 && $yvert == 1  && $zvert != 1 && $ifld > 1 && $jfld > 1 ) {
      print OUT "X($xunit),Y($yunit),Volume($nvunit)";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT ",$fvname[$i]($fvunit[$i])";
        } else {
          print OUT ",$fvname[$i]";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/4.;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/4.;
#        print OUT "$xs($nv[$i]) $ys($nv[$i])";
        print OUT "$xs,$ys,$nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT ",$fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction and Z-direction vertices only
#
    } elsif( $xvert == 1 && $zvert == 1  && $yvert != 1 && $ifld > 1 && $kfld > 1 ) {
      print OUT "X($xunit),Z($zunit),Volume($nvunit)";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT ",$fvname[$i]($fvunit[$i])";
        } else {
          print OUT ",$fvname[$i]";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/4.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/4.;
#        print OUT "$xs($nv[$i]) $zs($nv[$i])";
        print OUT "$xs,$zs,$nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT ",$fv[$k]";
        }
        print OUT "\n";
      }
#
#  Y-direction and Z-direction vertices only
#
    } elsif( $yvert == 1 && $zvert == 1  && $xvert != 1 && $jfld > 1 && $kfld > 1 ) {
      print OUT "Y($yunit),Z($zunit),Volume($nvunit)";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT ",$fvname[$i]($fvunit[$i])";
        } else {
          print OUT ",$fvname[$i]";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/4.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/4.;
#        print OUT "$ys($nv[$i]) $zs($nv[$i])";
        print OUT "$ys,$zs,$nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT ",$fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction, Y-direction and Z-direction vertices only
#
    } else {
      print OUT "X($xunit),Y($yunit),Z($zunit),Volume($nvunit)";
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT ",$fvname[$i]($fvunit[$i])";
        } else {
          print OUT ",$fvname[$i]";
        }
      }
      print OUT "\n";
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $j5 = $j1+4;
        $j6 = $j1+5;
        $j7 = $j1+6;
        $j8 = $j1+7;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/8.;
        $xs += ($xp[$j5]+$xp[$j6]+$xp[$j7]+$xp[$j8])/8.;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/8.;
        $ys += ($yp[$j5]+$yp[$j6]+$yp[$j7]+$yp[$j8])/8.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/8.;
        $zs += ($zp[$j5]+$zp[$j6]+$zp[$j7]+$zp[$j8])/8.;
#        print OUT "$xs($nv[$i]) $ys($nv[$i]) $zs($nv[$i])";
        print OUT "$xs,$ys,$zs,$nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT ",$fv[$k]";
        }
        print OUT "\n";
      }
    }
  }
#
#  Write MatLab output file
#
  if( $plot_package =~ /^matlab\b/i ) {
#
#  X-direction vertices only
#
    if( $xvert == 1 && $ifld > 1 && $jfld == 1 && $kfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $xs = ($xp[$j1]+$xp[$j2])/2.;
        print OUT "$xs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  Y-direction vertices only
#
    } elsif( $yvert == 1 && $jfld > 1 && $ifld == 1 && $kfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $ys = ($yp[$j1]+$yp[$j2])/2.;
        print OUT "$ys $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  Z-direction vertices only 
#
    } elsif( $zvert == 1 && $kfld > 1 && $ifld == 1 && $jfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $zs = ($zp[$j1]+$zp[$j2])/2.;
        print OUT "$zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction and Y-direction vertices only
#
    } elsif( $xvert == 1 && $yvert == 1  && $ifld > 1 && $jfld > 1 && $kfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/4.;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/4.;
        print OUT "$xs $ys $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction and Y-direction vertices only
#
    } elsif( $xvert == 1 && $zvert == 1  && $ifld > 1 && $kfld > 1 && $jfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/4.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/4.;
        print OUT "$xs $zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  Y-direction and Z-direction vertices only
#
    } elsif( $yvert == 1 && $zvert == 1  && $jfld > 1 && $kfld > 1 && $ifld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/4.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/4.;
        print OUT "$ys $zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
#
#  X-direction, Y-direction and Z-direction vertices only
#
    } else {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $j3 = $j1+2;
        $j4 = $j1+3;
        $j5 = $j1+4;
        $j6 = $j1+5;
        $j7 = $j1+6;
        $j8 = $j1+7;
        $xs = ($xp[$j1]+$xp[$j2]+$xp[$j3]+$xp[$j4])/8.;
        $xs += ($xp[$j5]+$xp[$j6]+$xp[$j7]+$xp[$j8])/8.;
        $ys = ($yp[$j1]+$yp[$j2]+$yp[$j3]+$yp[$j4])/8.;
        $ys += ($yp[$j5]+$yp[$j6]+$yp[$j7]+$yp[$j8])/8.;
        $zs = ($zp[$j1]+$zp[$j2]+$zp[$j3]+$zp[$j4])/8.;
        $zs += ($zp[$j5]+$zp[$j6]+$zp[$j7]+$zp[$j8])/8.;
        print OUT "$xs $ys $zs $nv[$i]";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
      print OUT "\n";
    }
  }
#
#  Write Gnuplot output file
#
  if( $plot_package =~ /^gnuplot\b/i ) {
    if( $ifld > 1 && $jfld > 1 && $kfld > 1 ) {
      die "Error: Three-Dimensional Plot\n";
    } elsif( $ifld > 1 && $jfld > 1 ) {
      $igrid = $ifld;
      $jgrid = $jfld;
      $xlabel = "x, " . $xunit;
      $ylabel = "y, " . $yunit;
      $splot = 1;
      for( $j = 0; $j < $jfld; $j++ ) {
        for( $i = 0; $i < $ifld; $i++ ) {
          $n = $j*$ifld + $i;
          print OUT "$xp[$n] $yp[$n] $nv[$n]";
          for( $jv = 0; $jv < $nfv; $jv++ ) {
            $kv = $jv*$nfld + $n;
            print OUT " $fv[$kv]";
          }
          print OUT "\n";
        }
        print OUT "\n";
      }
      print OUT "\n";
      print OUT "\n";
    } elsif( $jfld > 1 && $kfld > 1 ) {
      $igrid = $jfld;
      $jgrid = $kfld;
      $xlabel = "y, " . $yunit;
      $ylabel = "z, " . $zunit;
      $splot = 1;
      for( $k = 0; $k < $kfld; $k++ ) {
        for( $j = 0; $j < $jfld; $j++ ) {
          $n = $k*$jfld + $j;
          print OUT "$yp[$n] $zp[$n] $nv[$n]";
          for( $jv = 0; $jv < $nfv; $jv++ ) {
            $kv = $jv*$nfld + $n;
            print OUT " $fv[$kv]";
          }
          print OUT "\n";
        }
        print OUT "\n";
      }
      print OUT "\n";
      print OUT "\n";
    } elsif( $kfld > 1 && $ifld > 1 ) {
      $igrid = $ifld;
      $jgrid = $kfld;
      $xlabel = "x, " . $xunit;
      $ylabel = "z, " . $zunit;
      $splot = 1;
      for( $k = 0; $k < $kfld; $k++ ) {
        for( $i = 0; $i < $ifld; $i++ ) {
          $n = $k*$ifld + $i;
          print OUT "$xp[$n] $zp[$n] $nv[$n]";
          for( $jv = 0; $jv < $nfv; $jv++ ) {
            $kv = $jv*$nfld + $n;
            print OUT " $fv[$kv]";
          }
          print OUT "\n";
        }
        print OUT "\n";
      }
      print OUT "\n";
      print OUT "\n";
    } elsif( $ifld > 1 ) {
      $igrid = $ifld;
      $xlabel = "x, " . $xunit;
      $plot = 1;
      for( $i = 0; $i < $ifld; $i++ ) {
        print OUT "$xp[$i] $nv[$i]";
        for( $jv = 0; $jv < $nfv; $jv++ ) {
          $kv = $jv*$nfld + $i;
          print OUT " $fv[$kv]";
        }
        print OUT "\n";
      }
      print OUT "\n";
      print OUT "\n";
    } elsif( $jfld > 1 ) {
      $igrid = $jfld;
      $xlabel = "y, " . $yunit;
      $plot = 1;
      for( $j = 0; $j < $jfld; $j++ ) {
        print OUT "$yp[$j] $nv[$j]";
        for( $jv = 0; $jv < $nfv; $jv++ ) {
          $kv = $jv*$nfld + $j;
          print OUT " $fv[$kv]";
        }
        print OUT "\n";
      }
      print OUT "\n";
      print OUT "\n";
    } elsif( $kfld > 1 ) {
      $igrid = $kfld;
      $xlabel = "z, " . $zunit;
      $plot = 1;
      for( $k = 0; $k < $kfld; $k++ ) {
        print OUT "$zp[$k] $nv[$k]";
        for( $jv = 0; $jv < $nfv; $jv++ ) {
          $kv = $jv*$nfld + $k;
          print OUT " $fv[$kv]";
        }
        print OUT "\n";
      }
      print OUT "\n";
      print OUT "\n";
    } else {
      die "Error: Zero-Dimensional Plot\n";
    }
  }
#
#  Write Tecplot output file
#
  if( $plot_package =~ /^tecplotxy\b/i ) {
    if( $nf == 0 ) {
      print OUT "TITLE = \"$title\"\n";
      if( $xvert == 1 && $ifld > 1 && $jfld == 1 && $kfld == 1 ) {
        print OUT "VARIABLES = \"X, $xunit\"";
      } elsif( $yvert == 1 && $jfld > 1 && $ifld == 1 && $kfld == 1 ) {
        print OUT "VARIABLES = \"Y, $yunit\"";
      } elsif( $zvert == 1 && $kfld > 1 && $ifld == 1 && $jfld == 1 ) {
        print OUT "VARIABLES = \"Z, $zunit\"";
      } else {
        die "Error: Non-One-Dimensional Plot\n";
      }
      for( $i = 0; $i <= $#fvname; $i++ ) {
        if( $fvunit[$i] ) {
          print OUT "\" $fvname[$i], $fvunit[$i]\"";
        } else {
          print OUT "\" $fvname[$i]\"";
        }
      }
      print OUT "\n";
    }
    if( $ts_opt == 1 ) {
      if( $ta_opt == 1 ) {
        $tsv[$nf] = $tsv[$nf] + $tadj;
      }
      print OUT "ZONE T = \"$tsv[$nf], $tsunit\" I = $nfld F = POINT";
    } else {
      print OUT "ZONE T = \"$plot_file[$nf]\" I = $nfld F = POINT";
    }
    print OUT "\n";
    if( $xvert == 1 && $ifld > 1 && $jfld == 1 && $kfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+1;
        $xs = ($xp[$j1]+$xp[$j2])/2.;
        print OUT "$xs";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
    } elsif( $yvert == 1 && $jfld > 1 && $ifld == 1 && $kfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+3;
        $ys = ($yp[$j1]+$yp[$j2])/2.;
        print OUT "$ys";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
    } elsif( $zvert == 1 && $kfld > 1 && $ifld == 1 && $jfld == 1 ) {
      for( $i = 0; $i < $nfld; $i++ ) {
        $j1 = $i*$nvert;
        $j2 = $j1+5;
        $zs = ($zp[$j1]+$zp[$j2])/2.;
        print OUT "$zs";
        for( $j = 0; $j < $nfv; $j++ ) {
          $k = $j*$nfld + $i;
          print OUT " $fv[$k]";
        }
        print OUT "\n";
      }
    }
  }
#
#  Write Tecplot output file
#
  if( $plot_package =~ /^tecplot\b/i || $plot_package =~ /^tecplot10\b/i ) {
    if( $plot_line == $plot_array[0] ) {
      if( $nf == 0 ) {
        print OUT "TITLE = \"$title\"\n";
        if( $xsurf*$ysurf*$zsurf ) {
          print OUT "VARIABLES = \"X, $xunit\" \"Y, $yunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
        } elsif( $xvert == 1 && $yvert == 1 && $zvert == 1 ) {
          print OUT "VARIABLES = \"X, $xunit\" \"Y, $yunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
        } elsif( $ifld > 1 && $jfld > 1 && $kfld > 1 ) {
          print OUT "VARIABLES = \"X, $xunit\" \"Y, $yunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
        } elsif( $ifld > 1 && $jfld > 1 ) {
          print OUT "VARIABLES = \"X, $xunit\" \"Y, $yunit\" \"Volume, $nvunit\"";
        } elsif( $jfld > 1 && $kfld > 1 ) {
          print OUT "VARIABLES = \"Y, $yunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
        } elsif( $kfld > 1 && $ifld > 1 ) {
          print OUT "VARIABLES = \"X, $xunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
        } elsif( $ifld > 1 ) {
          print OUT "VARIABLES = \"X, $xunit\" \"Y, $yunit\" \"Volume, $nvunit\"";
        } elsif( $jfld > 1 ) {
          print OUT "VARIABLES = \"Y, $yunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
        } elsif( $kfld > 1 ) {
          print OUT "VARIABLES = \"X, $xunit\" \"Z, $zunit\" \"Volume, $nvunit\"";
        } else {
          die "Error: Single Node Plot\n";
        }
        for( $i = 0; $i <= $#fvname; $i++ ) {
          if( $fvunit[$i] ) {
            print OUT "\" $fvname[$i], $fvunit[$i]\"";
          } else {
            print OUT "\" $fvname[$i]\"";
          }
        }
        print OUT "\n";
      }
    }
    if( $ts_opt == 1 ) {
      if( $ta_opt == 1 ) {
        $tsv[$nf] = $tsv[$nf] + $tadj;
      }
      print OUT "ZONE T = \"$tsv[$nf], $tsunit\", STRANDID = 1, SOLUTIONTIME = $tsv[$nf] ";
    } else {
      print OUT "ZONE T = \"$plot_file[$nf]\", ";
    }
    if( $xvert == 1 && $yvert == 1 && $zvert == 1 ) {
      $nsum = $nfld+$nbrn;
      $nnodes = $nvert*$nsum;
      if( $plot_package =~ /^tecplot10\b/i ) {
        print OUT "N = $nnodes, E = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEBRICK";
      } else {
      print OUT "NODES = $nnodes, ELEMENTS = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEBRICK";
      }  
      if( $nf > 0 ) {
        print OUT " VARSHARELIST = ([1,2,3]=1), CONNECTIVITYSHAREZONE = 1\n";
      } else {
        print OUT "\n";
      }
      print OUT "VARLOCATION=([4";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 5;
        print OUT ",$k";
      }
      print OUT "]=CELLCENTERED";
    } elsif( $xvert == 1 && $yvert == 1 ) {
      $nsum = $nfld+$nbrn;
      $nnodes = $nvert*$nsum;
      if( $plot_package =~ /^tecplot10\b/i ) {
        print OUT "N = $nnodes, E = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEQUADRILATERAL";
      } else {
      print OUT "NODES = $nnodes, ELEMENTS = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEQUADRILATERAL";
      }
      if( $nf > 0 ) {
        print OUT " VARSHARELIST = ([1,2]=1), CONNECTIVITYSHAREZONE = 1\n";
      } else {
        print OUT "\n";
      }
      print OUT "VARLOCATION=([3";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ",$k";
      }
      print OUT "]=CELLCENTERED";
    } elsif( $xvert == 1 && $zvert == 1 ) {
      $nsum = $nfld+$nbrn;
      $nnodes = $nvert*$nsum;
      if( $plot_package =~ /^tecplot10\b/i ) {
        print OUT "N = $nnodes, E = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEQUADRILATERAL";
      } else {
      print OUT "NODES = $nnodes, ELEMENTS = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEQUADRILATERAL";
      }
      if( $nf > 0 ) {
        print OUT " VARSHARELIST = ([1,2]=1), CONNECTIVITYSHAREZONE = 1\n";
      } else {
        print OUT "\n";
      }
      print OUT "VARLOCATION=([3";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ",$k";
      }
      print OUT "]=CELLCENTERED";
    } elsif( $yvert == 1 && $zvert == 1 ) {
      $nsum = $nfld+$nbrn;
      $nnodes = $nvert*$nsum;
      if( $plot_package =~ /^tecplot10\b/i ) {
        print OUT "N = $nnodes, E = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEQUADRILATERAL";
      } else {
      print OUT "NODES = $nnodes, ELEMENTS = $nactn, DATAPACKING = BLOCK, ZONETYPE = FEQUADRILATERAL";
      }
      if( $nf > 0 ) {
        print OUT " VARSHARELIST = ([1,2]=1), CONNECTIVITYSHAREZONE = 1\n";
      } else {
        print OUT "\n";
      }
      print OUT "VARLOCATION=([3";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ",$k";
      }
      print OUT "]=CELLCENTERED";
    } elsif( $xsurf*$ysurf*$zsurf ) {
      print OUT "I = $ifldx, J = $jfldx, K = $kfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(4=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 5;
        print OUT ", $k=CELLCENTERED";
      }
    } elsif( $ifld > 1 && $jfld > 1 && $kfld > 1 ) {
      print OUT "I = $ifldx, J = $jfldx, K = $kfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(4=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 5;
        print OUT ", $k=CELLCENTERED";
      }
    } elsif( $ifld > 1 && $jfld > 1 ) {
      print OUT "I = $ifldx, J = $jfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(3=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ", $k=CELLCENTERED";
      }
    } elsif( $jfld > 1 && $kfld > 1 ) {
      print OUT "I = $jfldx, J = $kfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(3=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ", $k=CELLCENTERED";
      }
    } elsif( $kfld > 1 && $ifld > 1 ) {
      print OUT "I = $ifldx, J = $kfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(3=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ", $k=CELLCENTERED";
      }
    } elsif( $ifld > 1 ) {
      print OUT "I = $ifldx, J = $jfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(3=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ", $k=CELLCENTERED";
      }
    } elsif( $jfld > 1 ) {
      print OUT "I = $ifldx, J = $kfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(3=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ", $k=CELLCENTERED";
      }
    } elsif( $kfld > 1 ) {
      print OUT "I = $ifldx, J = $kfldx, DATAPACKING = BLOCK,\n";
      print OUT "VARLOCATION=(3=CELLCENTERED";
      for( $j = 0; $j < $nfv; $j++ ) {
        $k = $j + 4;
        print OUT ", $k=CELLCENTERED";
      }
    } else {
      die "Error: Single Node Plot\n";
    }
    print OUT ")\n";
#
#   Tecplot XYZ FEBRICK with vertices
#
    if( $nf == 0 && $xvert == 1 && $yvert == 1 && $zvert == 1 ) {
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$xp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$yp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$zp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
#
#   Tecplot XY FEQUADRILATERAL with vertices
#
    } elsif( $nf == 0 && $xvert == 1 && $yvert == 1 ) {
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$xp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$yp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
#
#   Tecplot XZ FEQUADRILATERAL with vertices
#
    } elsif( $nf == 0 && $xvert == 1 && $zvert == 1 ) {
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$xp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$zp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
#
#   Tecplot YZ FEQUADRILATERAL with vertices
#
    } elsif( $nf == 0 && $yvert == 1 && $zvert == 1 ) {
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$yp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $n = 0; $n < $nsum; $n++ ) {
        for( $m = 0; $m < $nvert; $m++ ) {
          print OUT "$zp[$nc] ";
          $nc++;
        }
        if( $nc > 0 ) {
          print OUT "\n";
        }
      }
      $nc = 0;
    } elsif( $nf == 0 ) {
      die "Error: Single Node Plot\n";
    }
    $nc = 0;
    $nsum = $nfld + $nbrn; 
    for( $i = 0; $i < $nsum; $i++ ) {
      if( $ixp[$i] != 0 ) {
        $nc++;
        print OUT "$nv[$i] ";
        if( $nc == 10 ) {
          print OUT "\n";
          $nc = 0;
        }
      }
    }
    if( $nc > 0 ) {
      print OUT "\n";
    }
    for( $n = 0; $n < $nfv; $n++ ) {
      $nc = 0;
      $nsum = $nfld + $nbrn; 
      for( $i = 0; $i < $nsum; $i++ ) {
        if( $ixp[$i] != 0 ) {
          $nc++;
          $k = $n*$nsum + $i;
          print OUT "$fv[$k] ";
          if( $nc == 10 ) {
            print OUT "\n";
            $nc = 0;
          }
        }
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
    }
#
#   Read Tecplot connectivity list from the 'connect' file
#
    if( $nf == 0 ) {
      open( CONNECT,"connect" ) || die "Error: Unable to Tecplot Connectivity List File: connect.\n";
      @connect_array = <CONNECT>;
#
#     Loop over lines in connect file
#
      foreach $connect_line (@connect_array) {
#
#       Remove return from line
#
        chomp( $connect_line );
#
#      Remove leading blank spaces from line
#
        $connect_line =~ s/^\s+//;
        print OUT "$connect_line\n";
      }
      close( CONNECT );
    }
  }
#
#  Write VTK output file
#
  $nnodes = $nvert*$nfld;
  if( $plot_package =~ /^vtk\b/i ) {
    if( $plot_line == $plot_array[0] ) {
      print OUT "# vtk DataFile Version 2.0\n";
      print OUT "Unstructured Grid\n";
      print OUT "ASCII\n";
      print OUT "DATASET UNSTRUCTURED_GRID\n";
      print OUT "POINTS $nnodes float\n";
    }
#
#   VTK Unstructured Grid Hexahedron
#
    $nc = 0;
    for( $n = 0; $n < $nfld; $n++ ) {
      for( $m = 0; $m < $nvert; $m++ ) {
        print OUT "$xp[$nc] $yp[$nc] $zp[$nc] ";
        $nc++;
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
    }
#
#   Read VTK connectivity list from the 'connect' file and write out cells
#
    open( CONNECT,"connect" ) || die "Error: Unable to Tecplot Connectivity List File: connect.\n";
    $nsize = $nactn*9;
    print OUT "\n";
    print OUT "CELLS $nactn $nsize\n";
    @connect_array = <CONNECT>;
#
#   Loop over lines in connect file
#
    foreach $connect_line (@connect_array) {
#
#     Remove return from line
#
      chomp( $connect_line );
#
#    Remove leading blank spaces from line
#
      $connect_line =~ s/^\s+//;
      @connect_points = split(/\s+/,$connect_line);
      print OUT "$nvert ";
      for( $i = 0; $i <= $#connect_points; $i++ ) {
        $c_one = $connect_points[$i]-1;
        print OUT "$c_one ";
      }
      print OUT "\n"; 
    }
    close( CONNECT );
# 
# Write out cell types (VTK_Hexahedron=12 for 3D)
#
    print OUT "\n";
    print OUT "CELL_TYPES $nactn\n";    
#    if( $ifld > 1 && $jfld > 1 && $kfld > 1 ) {
      for( $m = 0; $m < $nactn; $m++ ) {
        print OUT "12\n";
      }
#    } elsif( ($ifld > 1 && $jfld > 1) || ($jfld > 1 && $kfld > 1) || ($kfld > 1 && $ifld > 1)  ) {
#      for( $m = 0; $m < $nactn; $m++ ) {
#        print OUT "9\n";
#      }
#    } else {
#      for( $m = 0; $m < $nactn; $m++ ) {
#        print OUT "12\n";
#      }
#    }
# 
# Write out field data as cell_data (cell centered)
#
    print OUT "\n";
    print OUT "CELL_DATA $nactn\n";
    print OUT "SCALARS Cell_Volume float\n";
    print OUT "LOOKUP_TABLE default\n";
    $nc = 0;
    for( $i = 0; $i < $nfld; $i++ ) {
      if( $ixp[$i] != 0 ) {
        $nc++;
        print OUT "$nv[$i] ";
        if( $nc == 10 ) {
          print OUT "\n";
          $nc = 0;
        }
      }
    }
    if( $nc > 0 ) {
      print OUT "\n";
    }
    print OUT "\n";
    for( $n = 0; $n <= $#fvname; $n++ ) {
      if( $fvunit[$n] ) {
#        @vtk_name = split(/\s+/,$fvname[$n]);
        $fvname[$n] =~ s/ /_/g;
        print OUT "SCALARS $fvname[$n]_$fvunit[$n] float\n";
        print OUT "LOOKUP_TABLE default\n";
      }else { 
        $fvname[$n] =~ s/ /_/g;
        print OUT "SCALARS $fvname[$n] float\n";
        print OUT "LOOKUP_TABLE default\n";
      }
      $nc = 0;
      for( $i = 0; $i < $nfld; $i++ ) {
        if( $ixp[$i] != 0 ) {
          $nc++;
          $k = $n*$nfld + $i;
          print OUT "$fv[$k] ";
          if( $nc == 10 ) {
            print OUT "\n";
            $nc = 0;
          }
        }
      }
      if( $nc > 0 ) {
        print OUT "\n";
      }
      print OUT "\n";
    }      
  }
  close( PLOT );
  @xp = ();
  @yp = ();
  @zp = ();
  @nv = ();
  @fv = ();
  if( $nf < $#plot_file ) {
    @fvname = ();
    @fvunit = ();
  }
}
close( OUT );
#
#  Write gnuplot scripts for each reference node variable
#
if( $plot_package =~ /^gnuplot\b/i ) {
  if( $res_opt ) {
    $igrid = $igrid*$res_fac;
    $jgrid = $jgrid*$res_fac;
  }
  for( $i = 0; $i <= $#fvname; $i++ ) {
    if( $fvname[$i] =~ /aqueous\b/i ) {
      if( $fvname[$i] =~ /pressure\b/i ){
        $abbrev = 'pl';
      } elsif( $fvname[$i] =~ /saturation\b/i ){
        $abbrev = 'sl';
      } elsif( $fvname[$i] =~ /moisture content\b/i ){
        $abbrev = 'mcl';
      } elsif( $fvname[$i] =~ /hydraulic head\b/i ){
        $abbrev = 'hhl';
      } elsif( $fvname[$i] =~ /darcy velocity\b/i ){
        if( $fvname[$i] =~ /x-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'ulnc';
          } else {
            $abbrev = 'ul';
          }
        } elsif( $fvname[$i] =~ /y-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'vlnc';
          } else {
            $abbrev = 'vl';
          }
        } elsif( $fvname[$i] =~ /z-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'wlnc';
          } else {
            $abbrev = 'wl';
          }
        }
      }
    } elsif( $fvname[$i] =~ /gas\b/i ) {
      if( $fvname[$i] =~ /pressure\b/i ){
        $abbrev = 'pg';
      } elsif( $fvname[$i] =~ /saturation\b/i ){
        $abbrev = 'sg';
      } elsif( $fvname[$i] =~ /moisture content\b/i ){
        $abbrev = 'mcg';
      } elsif( $fvname[$i] =~ /hydraulic head\b/i ){
        $abbrev = 'hhg';
      } elsif( $fvname[$i] =~ /darcy velocity\b/i ){
        if( $fvname[$i] =~ /x-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'ugnc';
          } else {
            $abbrev = 'ug';
          }
        } elsif( $fvname[$i] =~ /y-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'vgnc';
          } else {
            $abbrev = 'vg';
          }
        } elsif( $fvname[$i] =~ /z-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'wgnc';
          } else {
            $abbrev = 'wg';
          }
        }
      }
    } elsif( $fvname[$i] =~ /napl\b/i ) {
      if( $fvname[$i] =~ /pressure\b/i ){
        $abbrev = 'pn';
      } elsif( $fvname[$i] =~ /saturation\b/i ){
        $abbrev = 'sn';
      } elsif( $fvname[$i] =~ /moisture content\b/i ){
        $abbrev = 'mcn';
      } elsif( $fvname[$i] =~ /hydraulic head\b/i ){
        $abbrev = 'hhn';
      } elsif( $fvname[$i] =~ /darcy velocity\b/i ){
        if( $fvname[$i] =~ /x-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'unnc';
          } else {
            $abbrev = 'un';
          }
        } elsif( $fvname[$i] =~ /y-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'vnnc';
          } else {
            $abbrev = 'vn';
          }
        } elsif( $fvname[$i] =~ /z-dir\b/i ){
          if( $fvname[$i] =~ /node centered\b/i ){
            $abbrev = 'wnnc';
          } else {
            $abbrev = 'wn';
          }
        }
      }
    }
    $abbrev = $fvname[$i];
    $abbrev =~ s/ /_/g;
    $abbrev =~ s/\(//g;
    $abbrev =~ s/\)//g;
    $abbrev =~ s/Aqueous/Aqu/g;
    $abbrev =~ s/Volumetric/Vol/g;
    $abbrev =~ s/Concentration/Conc/g;

    $gnu_file = $abbrev . "_plot.gnu";

    if( $abbrev ) {
      $gnu_file = $abbrev . "_plot.gnu";
      $abbrev = "";
    } else {
      $n = $i + 1;
      $gnu_file = "v" . $n . "_plot.gnu";
    }
    open( GNU,">$gnu_file") || die "Error: Unable to Open Gnuplot Script File: $gnu_file.\n";
    print GNU "set term x11 font \"Times,16\"\n";
    if( $title ) {
      print GNU "set title \"$title\"\n";
    }
    if( $splot ) {
      print GNU "set xlabel \"$xlabel\"\n";
      print GNU "set ylabel \"$ylabel\"\n";
      print GNU "set pm3d map\n";
      print GNU "set cntrparam levels 100\n";
      print GNU "set dgrid3d $igrid,$jgrid\n";
      print GNU "set palette color positive\n";
      for( $nf = 0; $nf <= $#plot_file; $nf++ ) {
        print GNU "splot [:] [:] \\\n";
        $k = $i+4;
        if( $ts_opt == 1 ) {
          $legend = $tsv[$nf] . ", " . $tsunit;
          print GNU "\'$out_file\' using 1:2:$k index $nf title \"$legend\" with lines lw 2 \n";
          print GNU "pause mouse \"Click to continue ...\"\n";
        } else {
          print GNU "\'$out_file\' using 1:2:$k index $nf \n";
          print GNU "pause mouse \"Click to continue ...\"\n";
        }
      }
    } elsif( $plot ) {
      print GNU "set xlabel \"$xlabel\"\n";
      print GNU "set ylabel \"$fvname[$i], $fvunit[$1]\"\n";
      print GNU "set key outside\n";
      if( $logx_opt ) {
        print GNU "set log x\n";
      }
      if( $logy_opt ) {
        print GNU "set log y\n";
      }
      print GNU "plot [:] [:] \\\n";
      for( $nf = 0; $nf <= $#plot_file; $nf++ ) {
        if( $ts_opt == 1 ) {
          $legend = $tsv[$nf] . ", " . $tsunit;
        } else {
          $legend = $plot_file[$nf];
        }
        $k = $i+3;
        if( $nf < $#plot_file ) {
          print GNU "\'$out_file\' using 1:$k index $nf title \"$legend\" with lines lw 2, \\\n";
        } else {
          print GNU "\'$out_file\' using 1:$k index $nf title \"$legend\" with lines lw 2\n";
        }
      }
      print GNU "pause mouse \"Click to continue ...\"\n";
    } else {
      die "Error: Unknown Plot Type\n";
    }
    print GNU "reset\n";
    close( GNU );
  }
}

