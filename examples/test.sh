#!/bin/bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD/../lib

function run() {
    ./fortran_write cyanoformaldehyde.xyz cc-pvtz 
    ./fortran_read
}

F1=$(tempfile)
F2=$(tempfile)

run > $F1
cat > $F2 << EOF
 Wrote file trex_file
 Description: Cyanoformaldehyde (Angstroms)
 Electrons:                    14  up,                    14  down
C      6.0    -0.91561483     0.00000000    -1.22522833
C      6.0    -0.01092218     0.00000000     1.39523176
N      7.0     0.64170260     0.00000000     3.48820326
O      8.0     0.50833684     0.00000000    -3.00337868
H      1.0    -2.97202213     0.00000000    -1.42565675
 Basis type: Gaussian
 
 CARBON
 S                   10
  1   0.82360000E+04     0.53100000E-03
  2   0.12350000E+04     0.41080000E-02
  3   0.28080000E+03     0.21087000E-01
  4   0.79270000E+02     0.81853000E-01
  5   0.25590000E+02     0.23481700E+00
  6   0.89970000E+01     0.43440100E+00
  7   0.33190000E+01     0.34612900E+00
  8   0.90590000E+00     0.39378000E-01
  9   0.36430000E+00    -0.89830000E-02
 10   0.12850000E+00     0.23850000E-02
 S                   10
  1   0.82360000E+04    -0.11300000E-03
  2   0.12350000E+04    -0.87800000E-03
  3   0.28080000E+03    -0.45400000E-02
  4   0.79270000E+02    -0.18133000E-01
  5   0.25590000E+02    -0.55760000E-01
  6   0.89970000E+01    -0.12689500E+00
  7   0.33190000E+01    -0.17035200E+00
  8   0.90590000E+00     0.14038200E+00
  9   0.36430000E+00     0.59868400E+00
 10   0.12850000E+00     0.39538900E+00
 S                    1
  1   0.90590000E+00     0.10000000E+01
 S                    1
  1   0.12850000E+00     0.10000000E+01
 P                    5
  1   0.18710000E+02     0.14031000E-01
  2   0.41330000E+01     0.86866000E-01
  3   0.12000000E+01     0.29021600E+00
  4   0.38270000E+00     0.50100800E+00
  5   0.12090000E+00     0.34340600E+00
 P                    1
  1   0.38270000E+00     0.10000000E+01
 P                    1
  1   0.12090000E+00     0.10000000E+01
 D                    1
  1   0.10970000E+01     0.10000000E+01
 D                    1
  1   0.31800000E+00     0.10000000E+01
 F                    1
  1   0.76100000E+00     0.10000000E+01
 
 CARBON
 S                   10
  1   0.82360000E+04     0.53100000E-03
  2   0.12350000E+04     0.41080000E-02
  3   0.28080000E+03     0.21087000E-01
  4   0.79270000E+02     0.81853000E-01
  5   0.25590000E+02     0.23481700E+00
  6   0.89970000E+01     0.43440100E+00
  7   0.33190000E+01     0.34612900E+00
  8   0.90590000E+00     0.39378000E-01
  9   0.36430000E+00    -0.89830000E-02
 10   0.12850000E+00     0.23850000E-02
 S                   10
  1   0.82360000E+04    -0.11300000E-03
  2   0.12350000E+04    -0.87800000E-03
  3   0.28080000E+03    -0.45400000E-02
  4   0.79270000E+02    -0.18133000E-01
  5   0.25590000E+02    -0.55760000E-01
  6   0.89970000E+01    -0.12689500E+00
  7   0.33190000E+01    -0.17035200E+00
  8   0.90590000E+00     0.14038200E+00
  9   0.36430000E+00     0.59868400E+00
 10   0.12850000E+00     0.39538900E+00
 S                    1
  1   0.90590000E+00     0.10000000E+01
 S                    1
  1   0.12850000E+00     0.10000000E+01
 P                    5
  1   0.18710000E+02     0.14031000E-01
  2   0.41330000E+01     0.86866000E-01
  3   0.12000000E+01     0.29021600E+00
  4   0.38270000E+00     0.50100800E+00
  5   0.12090000E+00     0.34340600E+00
 P                    1
  1   0.38270000E+00     0.10000000E+01
 P                    1
  1   0.12090000E+00     0.10000000E+01
 D                    1
  1   0.10970000E+01     0.10000000E+01
 D                    1
  1   0.31800000E+00     0.10000000E+01
 F                    1
  1   0.76100000E+00     0.10000000E+01
 
 NITROGEN
 S                   10
  1   0.11420000E+05     0.52300000E-03
  2   0.17120000E+04     0.40450000E-02
  3   0.38930000E+03     0.20775000E-01
  4   0.11000000E+03     0.80727000E-01
  5   0.35570000E+02     0.23307400E+00
  6   0.12540000E+02     0.43350100E+00
  7   0.46440000E+01     0.34747200E+00
  8   0.12930000E+01     0.41262000E-01
  9   0.51180000E+00    -0.85080000E-02
 10   0.17870000E+00     0.23840000E-02
 S                   10
  1   0.11420000E+05    -0.11500000E-03
  2   0.17120000E+04    -0.89500000E-03
  3   0.38930000E+03    -0.46240000E-02
  4   0.11000000E+03    -0.18528000E-01
  5   0.35570000E+02    -0.57339000E-01
  6   0.12540000E+02    -0.13207600E+00
  7   0.46440000E+01    -0.17251000E+00
  8   0.12930000E+01     0.15181400E+00
  9   0.51180000E+00     0.59994400E+00
 10   0.17870000E+00     0.38746200E+00
 S                    1
  1   0.12930000E+01     0.10000000E+01
 S                    1
  1   0.17870000E+00     0.10000000E+01
 P                    5
  1   0.26630000E+02     0.14670000E-01
  2   0.59480000E+01     0.91764000E-01
  3   0.17420000E+01     0.29868300E+00
  4   0.55500000E+00     0.49848700E+00
  5   0.17250000E+00     0.33702300E+00
 P                    1
  1   0.55500000E+00     0.10000000E+01
 P                    1
  1   0.17250000E+00     0.10000000E+01
 D                    1
  1   0.16540000E+01     0.10000000E+01
 D                    1
  1   0.46900000E+00     0.10000000E+01
 F                    1
  1   0.10930000E+01     0.10000000E+01
 
 OXYGEN
 S                   10
  1   0.15330000E+05     0.50800000E-03
  2   0.22990000E+04     0.39290000E-02
  3   0.52240000E+03     0.20243000E-01
  4   0.14730000E+03     0.79181000E-01
  5   0.47550000E+02     0.23068700E+00
  6   0.16760000E+02     0.43311800E+00
  7   0.62070000E+01     0.35026000E+00
  8   0.17520000E+01     0.42728000E-01
  9   0.68820000E+00    -0.81540000E-02
 10   0.23840000E+00     0.23810000E-02
 S                   10
  1   0.15330000E+05    -0.11500000E-03
  2   0.22990000E+04    -0.89500000E-03
  3   0.52240000E+03    -0.46360000E-02
  4   0.14730000E+03    -0.18724000E-01
  5   0.47550000E+02    -0.58463000E-01
  6   0.16760000E+02    -0.13646300E+00
  7   0.62070000E+01    -0.17574000E+00
  8   0.17520000E+01     0.16093400E+00
  9   0.68820000E+00     0.60341800E+00
 10   0.23840000E+00     0.37876500E+00
 S                    1
  1   0.17520000E+01     0.10000000E+01
 S                    1
  1   0.23840000E+00     0.10000000E+01
 P                    5
  1   0.34460000E+02     0.15928000E-01
  2   0.77490000E+01     0.99740000E-01
  3   0.22800000E+01     0.31049200E+00
  4   0.71560000E+00     0.49102600E+00
  5   0.21400000E+00     0.33633700E+00
 P                    1
  1   0.71560000E+00     0.10000000E+01
 P                    1
  1   0.21400000E+00     0.10000000E+01
 D                    1
  1   0.23140000E+01     0.10000000E+01
 D                    1
  1   0.64500000E+00     0.10000000E+01
 F                    1
  1   0.14280000E+01     0.10000000E+01
 
 HYDROGEN
 S                    5
  1   0.33870000E+02     0.60680000E-02
  2   0.50950000E+01     0.45308000E-01
  3   0.11590000E+01     0.20282200E+00
  4   0.32580000E+00     0.50390300E+00
  5   0.10270000E+00     0.38342100E+00
 S                    1
  1   0.32580000E+00     0.10000000E+01
 S                    1
  1   0.10270000E+00     0.10000000E+01
 P                    1
  1   0.14070000E+01     0.10000000E+01
 P                    1
  1   0.38800000E+00     0.10000000E+01
 D                    1
  1   0.10570000E+01     0.10000000E+01
EOF

exec diff $F1 $F2


