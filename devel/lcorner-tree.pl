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
use Math::PlanePath::LCornerTree;
use List::Util 'min', 'max';
use Math::PlanePath::Base::Digits
  'round_down_pow',
  'bit_split_lowtohigh',
  'digit_split_lowtohigh',
  'digit_join_lowtohigh';

{
  my $path = Math::PlanePath::LCornerTree->new (parts => 'diagonal-1');
  foreach my $depth (0 .. 20) {
    my $n = $path->tree_depth_to_n($depth);
    print "$n,";
  }
  print "\n";

  require Math::PlanePath::LCornerTreeByCells;
  $path = Math::PlanePath::LCornerTreeByCells->new (parts => 'diagonal-1');
  foreach my $depth (0 .. 20) {
    my $n = $path->tree_depth_to_n($depth);
    print "$n,";
  }
  print "\n";
  exit 0;
}
