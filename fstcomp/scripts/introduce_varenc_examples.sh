#!/bin/sh

cd ../examples


#
# C variability encoding examples
#

mkdir VarEncC
cd VarEncC
wget http://www.infosun.fim.uni-passau.de/spl/FAV/_static/download/ICSE13/Productline_features_C.zip
unzip Productline_features_C.zip
cd ..

cat >VarEncC/email/maxproduct.features <<EOL
Base
Keys
Encrypt
Decrypt
Sign
Verify
Forward
AddressBook
AutoResponder
EOL

cat >VarEncC/elevator/maxproduct.features <<EOL
base
weight
empty
overloaded
twothirdsfull
executivefloor
EOL

cat >VarEncC/minepump/maxproduct.features <<EOL
base
highWaterSensor
lowWaterSensor
methaneAlarm
methaneQuery
startCommand
stopCommand
EOL


#
# JAVA variability encoding examples
#

mkdir VarEncJava
cd VarEncJava
wget http://www.infosun.fim.uni-passau.de/spl/FAV/_static/download/ICSE13/Productline_features_Java.zip
unzip Productline_features_Java.zip
cd ..

cat >VarEncJava/email/maxproduct.features <<EOL
Base
Keys
Encrypt
Decrypt
Sign
Verify
Forward
AddressBook
AutoResponder
EOL

cat >VarEncJava/elevator/maxproduct.features <<EOL
base
weight
empty
overloaded
twothirdsfull
executivefloor
EOL

cat >VarEncJava/minepump/maxproduct.features <<EOL
base
highWaterSensor
lowWaterSensor
methaneAlarm
methaneQuery
startCommand
stopCommand
EOL

cat >VarEncJava/zipme/maxproduct.features <<EOL
Base
ZipMeTest
Compress
Extract
CRC
Adler32Checksum
GZIP
DerivativeCompressAdler32Checksum
DerivativeCompressCRC
DerivativeCompressGZIP
DerivativeCompressGZIPCRC
DerivativeExtractCRC
DerivativeGZIPCRC
EOL
