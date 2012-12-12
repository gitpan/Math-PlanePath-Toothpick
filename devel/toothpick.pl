#!/usr/bin/perl -w

# Copyright 2012 Kevin Ryde

# This file is part of Math-PlanePath-Toothpick.
#
# Math-PlanePath-Toothpick is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Math-PlanePath-Toothpick is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with Math-PlanePath-Toothpick.  If not, see <http://www.gnu.org/licenses/>.

use 5.004;
use strict;
use List::Util 'min', 'max';

# uncomment this to run the ### lines
#use Smart::Comments;


{
  # tree_n_to_depth()
  require Math::PlanePath::ToothpickTree;
  my $path = Math::PlanePath::ToothpickTree->new (parts => 2);
  my $prev = -999;
  my $count = 0;
  my $total = 0;
  for (my $n = 0; $n <= 256; $n++) {
    my $depth = $path->tree_n_to_depth($n);
    if ($depth != $prev) {
      print "$depth n=$n  added=$count  $total\n";
      $count = 0;
      $prev = $depth;
    }
    $count++;
    $total++;
  }
  exit 0;
}

{
  #         |               |
  # 6|     23--19--  --18--22
  #  |      |   |       |   |
  # 5|         16--13--15
  #  |              |   |
  # 4|-10--  ---9--11
  #  |  |       |   |   |
  # 3|  8---5---7 -12--14
  #  |      |   |       |   |
  # 2| -2---3      20--17--21
  #  |  |   |   |   |       |
  # 1|--1 --4---6
  #  |  |       |
  # 0|
  #  +--------------------
  #     0

  #         |               |
  # 6|     10---9--  ---9--10
  #  |      |   |       |   |
  # 5|          8---7---8
  #  |              |   |
  # 4|--5--  ---5---6
  #  |  |       |   |   |
  # 3|  4---3---4 --7---8
  #  |      |   |       |   |
  # 2| -1---2      10---9--10
  #  |  |   |   |   |       |
  # 1|--0 --3---4
  #  |  |       |
  # 0|
  #  +--------------------
  #     0

  # total 1-quad A153000 OFFSET=0
  #   0,1,2, 3,5,8,10, 11,13,16,19,23,30,38,42,
  #   0 1 2  3 4 5 6   7  8  9  10 11 12 13 14
  #
  #   43,45,48,51,55,62,70,75,79,86,95,105,120,142,162,170, 171,173,176,
  #   15 16 17 18 19 20 21 22 23 24 25 26  27  28  29  30   31  32  33
  #      +2 +3 +3 +4 +7 +8 +5 +4 +7 +9 +10 +15 +22 +20 +18  +1  +2  +
  #
  #  43 = 
  #  45 = 44 + 1
  #  48 = 44 + 2*0+1 = +4      
  #  51 = 44 + 2*2+3 = +7  +3  
  #  55   44 + 2*3+5 = +11 +4  
  #  62 = 44   2*5+8 = +18 +7  
  #  70 = 44 + 2*8+10 = +26 +8 
  #  75 = 44 + 2*10+11 = +31 +5
  #  79 = 44 + 2*11+13
  # 162 = 44 + 2*38+42
  # 170 = 44 + 2*42+42
  # 171
  #
  my @total = (0,0,1,2,3,5,8,10);
  print join(',',@total),"\n";
 OUTER: for (my $len = 8; ; $len *= 2) {
    my $t = $total[-1] + $len/8;
    # push @total, $t;
    print "[t=$t] ";
    for my $i (0 .. $len-1) {
      my $nt = $t + 2*$total[max($i,0)] + $total[min($i+1,$len-1)];
      # print "<$total[$i+1],$total[$i]>";
      print "$nt,";
      push @total, $nt;

      last OUTER if $#total > 33;
    }

    my $tsize = scalar(@total);
    print "  [tsize=$tsize]\n";
  }
  print "\n";
  exit 0;
}

{
  # added 4-quads
  my @added = (0, 1, 2, 4);
  print "0,1,\n2,4,\n";
  for (my $len = 4; $len <= 16; $len *= 2) {
    my $add = $len;
    push @added, $add;
    print "$add,";
    for my $i (1 .. $len-1) {
      my $add = $added[$i+1] + 2*$added[$i];
      print "$add,";
      push @added, $add;
    }
    my $asize = scalar(@added);
    print "  [asize=$asize]\n";
  }
  exit 0;
}
{
  #  0;
  #  1;
  #  2,4;
  #  4,4,8,12;
  #  8,4,8,12,12,16,28,32;
  # 16,4,8,12,12,16,28,32,20,16,28,36,40,60,88,80;
  # 32,4,8,12,12,16,28,32,20,16,28,36,40,60,88,80,36,16,28,36,40,60,88,84,56,..

  # 1, 1,
  # 1, 2, 3, 2,
  # 1, 2, 3, 3, 4, 7, 8, 4,
  # 1, 2, 3, 3, 4, 7, 8, 5, 4, 7, 9, 10, 15, 22, 20, 8,
  # 1, 2, 3, 3, 4, 7, 8, 5, 4, 7, 9, 10, 15, 22, 20, 9, 4, 7, 9, 10, 15, 22, 21, 14, 15, 23, 28, 35, 52, 64, 48, 16,
  # 1, 2, 3, 3, 4, 7, 8, 5, 4, 7, 9, 10, 15, 22, 20, 9, 4, 7, 9, 10, 15, 22, 21, 14, 15, 23

  #   0,
  #   1, 2,
  #   4, 4,
  #   4, 8, 12, 8,
  #   4, 8, 12, 12, 16, 28, 32, 16,
  #   4, 8, 12, 12, 16, 28, 32, 20, 16, 28, 36, 40, 60, 88, 80, 32,
  #   4, 8, 12, 12, 16, 28, 32, 20, 16, 28, 36, 40, 60, 88, 80, 36, 16, 28, 36, 40, 60, 88, 84, 56, 60, 92, 112, 140, 208, 256, 192, 64,
  #   4, 8, 12, 12, 16, 28, 32, 20, 16, 28

  my @add = (0,1);
  my $dpower = 2;
  my $d = 0;
  my $n = 1000;
  for (;;) {
    my $add;
    ### $d
    ### $dpower
    if ($d == 0) {
      $add = $dpower;
    } else {
      $add = 2*$add[$d] + $add[$d+1];
    }
    if (++$d >= $dpower) {
      $dpower *= 2;
      $d = 0;
    }
    ### $add
    if ($n <= $add) {
      last;
    }
    $n -= $add;
    push @add, $add;
  }
  print join(',',@add);
  exit 0;
}


