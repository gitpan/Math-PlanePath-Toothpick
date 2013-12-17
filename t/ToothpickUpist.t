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

use 5.004;
use strict;
use Test;
plan tests => 13;

use lib 't';
use MyTestHelpers;
MyTestHelpers::nowarnings();

# uncomment this to run the ### lines
#use Smart::Comments;

require Math::PlanePath::ToothpickUpist;


#------------------------------------------------------------------------------
# VERSION

{
  my $want_version = 13;
  ok ($Math::PlanePath::ToothpickUpist::VERSION, $want_version,
      'VERSION variable');
  ok (Math::PlanePath::ToothpickUpist->VERSION,  $want_version,
      'VERSION class method');

  ok (eval { Math::PlanePath::ToothpickUpist->VERSION($want_version); 1 },
      1,
      "VERSION class check $want_version");
  my $check_version = $want_version + 1000;
  ok (! eval { Math::PlanePath::ToothpickUpist->VERSION($check_version); 1 },
      1,
      "VERSION class check $check_version");

  my $path = Math::PlanePath::ToothpickUpist->new;
  ok ($path->VERSION,  $want_version, 'VERSION object method');

  ok (eval { $path->VERSION($want_version); 1 },
      1,
      "VERSION object check $want_version");
  ok (! eval { $path->VERSION($check_version); 1 },
      1,
      "VERSION object check $check_version");
}


#------------------------------------------------------------------------------
# n_start, x_negative, y_negative

{
  my $path = Math::PlanePath::ToothpickUpist->new;
  ok ($path->n_start, 0, 'n_start()');
  ok ($path->x_negative, 1, 'x_negative()');
  ok ($path->y_negative, 0, 'y_negative()');

  my @pnames = map {$_->{'name'}} $path->parameter_info_list;
  ok (join(',',@pnames), '', 'parameter_info_list() keys');
}

#------------------------------------------------------------------------------
# tree_depth_to_n(), tree_n_to_depth()

{
  my $pos_infinity = (2**256)**256;
  my $path = Math::PlanePath::ToothpickUpist->new;
  {
    my $n = $path->tree_depth_to_n($pos_infinity);
    ok (defined $n, 1, 'tree_depth_to_n(pos_infinity)');
  }
  {
    my $depth = $path->tree_n_to_depth($pos_infinity);
    ok (defined $depth, 1, 'tree_n_to_depth(pos_infinity)');
  }
}

#------------------------------------------------------------------------------
exit 0;
