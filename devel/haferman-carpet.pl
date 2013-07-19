#!/usr/bin/perl -w

# Copyright 2013 Kevin Ryde

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

use 5.010;
use strict;
use List::Util 'min', 'max';
use Math::PlanePath::Base::Digits
  'round_down_pow',
  'bit_split_lowtohigh',
  'digit_split_lowtohigh',
  'digit_join_lowtohigh';


{
  # carpet in .png image file

  require Image::Base::PNGwriter;
  my $scale = 1;
  my $depth = 6;
  my $width = $scale*3**$depth;
  my $height = $scale*3**$depth;
  my $image = Image::Base::PNGwriter->new (-width => $width,
                                           -height => $height);
  my ($draw0,$draw1);
  $draw0 = sub {
    my ($x,$y,$len) = @_;
    if ($len/3 <= $scale) {
      $image->rectangle($x,$y,$x+$len-1,$y+$len-1, '#000000');
    } else {
      $len /= 3;
      foreach my $xo (0 .. 2) {
        foreach my $yo (0 .. 2) {
          $draw1->($x+$xo*$len,$y+$yo*$len,$len);
        }
      }
    }
  };
   my @table = (1,0,1, 0,0,0, 1,0,1);
  # my @table = (1,0,1, 0,1,0, 1,0,1);
  # my @table = (0,1,0, 1,0,1, 0,1,0);
  $draw1 = sub {
    my ($x,$y,$len) = @_;
    if ($len/3 <= $scale) {
      $image->rectangle($x,$y,$x+$len-1,$y+$len-1, '#FFFFFF');
    } else {
      $len /= 3;
      foreach my $xo (0 .. 2) {
        foreach my $yo (0 .. 2) {
          my $func = ($table[$xo+3*$yo] ? $draw1 : $draw0);
          $func->($x+$xo*$len,$y+$yo*$len,$len);
        }
      }
    }
  };

  $draw1->(0,0, 3**$depth);
  # $image->save('/dev/stdout');
  $image->save('/tmp/x.png');
  system('ls -l /tmp/x.png');
  system('xzgv /tmp/x.png');
  exit 0;
}
