#!/bin/sh

if [ $# -ne 1 ]; then
    echo "usage: $0 <dir>"
    exit 2
fi

initdir=$PWD
mkdir -p $1
cd $1
if [ $? -ne 0 ]; then
    echo "'cd $1' failed"
    exit 2
fi

optionally_get() {
    cat <<EOF > Makefile
$1:
	$2
EOF
    make

}


# http://www.stearns.org/pcap/
optionally_get gre-sample.pcap "wget http://www.stearns.org/pcap/gre-sample.pcap"

optionally_get fuzz-2010-08-10-14745.pcap \
  "wget ftp://wireshark.org/automated/captures/fuzz-2010-08-10-14745.pcap"

# https://www.openpacket.org/capture/show/26
optionally_get malformed-arp.pcap "\
	wget https://www.openpacket.org/capture/grab/26 && \
	mv 26 malformed-arp.pcap"

rm Makefile

cd $initdir
