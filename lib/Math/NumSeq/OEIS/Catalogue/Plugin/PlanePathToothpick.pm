# Copyright 2011, 2012, 2013 Kevin Ryde

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

package Math::NumSeq::OEIS::Catalogue::Plugin::PlanePathToothpick;
use 5.004;
use strict;

use vars '$VERSION', '@ISA';
$VERSION = 6;
use Math::NumSeq::OEIS::Catalogue::Plugin;
@ISA = ('Math::NumSeq::OEIS::Catalogue::Plugin');

use constant info_arrayref =>
  [

   #---------------------------------------------------------------------------
   # LCornerTree

   { anum => 'A160410',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'LCornerTree',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A160412',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'LCornerTree,parts=3',
                     line_type => 'Depth_start',
                   ],
   },

   #---------------------------------------------------------------------------
   # LCornerReplicate

   { anum => 'A062880',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'LCornerReplicate',
                     line_type => 'Diagonal',
                   ],
   },

   #---------------------------------------------------------------------------
   # OneOfEight

   { anum => 'A151725',  # total
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'OneOfEight',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A151735',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'OneOfEight,parts=1',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A170880',  # V2=3mid
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'OneOfEight,parts=3mid',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A170879',  # V1=3side
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'OneOfEight,parts=3side',
                     line_type => 'Depth_start',
                   ],
   },

   #---------------------------------------------------------------------------
   # ToothpickTree

   { anum => 'A139250',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'ToothpickTree',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A153006',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'ToothpickTree,parts=3',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A152998',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'ToothpickTree,parts=2',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A153000',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'ToothpickTree,parts=1',
                     line_type => 'Depth_start',
                   ],
   },
   { anum => 'A160406',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'ToothpickTree,parts=wedge',
                     line_type => 'Depth_start',
                   ],
   },

   #---------------------------------------------------------------------------
   # ToothpickUpist

   { anum => 'A151566',
     class => 'Math::NumSeq::PlanePathN',
     parameters => [ planepath => 'ToothpickUpist',
                     line_type => 'Depth_start',
                   ],
   },

   #---------------------------------------------------------------------------
  ];

1;
__END__
