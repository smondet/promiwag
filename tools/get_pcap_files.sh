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
    echo "Optionally getting $1 from $2"
    cat <<EOF > Makefile
$1:
	$2
EOF
    make

}

optionally_get_from_seb() {
    local url="http://smondet.at.ifi.uio.no/resources/pcap/$1"
    optionally_get $1 "wget $url"

}

# http://www.stearns.org/pcap/
optionally_get gre-sample.pcap "wget http://www.stearns.org/pcap/gre-sample.pcap"

optionally_get fuzz-2010-08-10-14745.pcap \
  "wget ftp://wireshark.org/automated/captures/fuzz-2010-08-10-14745.pcap"

# https://www.openpacket.org/capture/show/26
optionally_get malformed-arp.pcap "\
	wget https://www.openpacket.org/capture/grab/26 && \
	mv 26 malformed-arp.pcap"

optionally_get_from_seb multigre_12_afew.pcap
optionally_get_from_seb multigre_12_two.pcap
optionally_get_from_seb multigre_24_afew.pcap
optionally_get_from_seb multigre_24_two.pcap


rm Makefile

cd $initdir
