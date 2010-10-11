#!/bin/bash
# Author: Beatrice Barbe
if test $# -ne 1 ; then
    echo "usage: $0 <tun_number>";
    exit 2;
fi
nber_tun=$1
x=1
REMOTE_NET="192.168.10.0"
REMOTE_NETMASK="255.255.255.0"
LOCAL_EXT="129.240.67.22"
REMOTE_EXT="192.168.10.1"
LOCAL_IP_TUNNEL="10.10."$x".9"
REMOTE_IP_TUNNEL="10.10."$x".10"
tun="tun"$x"9"
modprobe ip_gre
ip tunnel add $tun mode gre local $LOCAL_EXT remote $REMOTE_EXT
ifconfig $tun $LOCAL_IP_TUNNEL pointopoint $REMOTE_IP_TUNNEL
while test $x -ne $nber_tun 
do
    x=$(($x+1))
    tun="tun"$x"9"
    LOCAL_EXT=$LOCAL_IP_TUNNEL
    REMOTE_EXT=$REMOTE_IP_TUNNEL
    LOCAL_IP_TUNNEL="10.10."$x".9"
    REMOTE_IP_TUNNEL="10.10."$x".10"
    ip tunnel add $tun mode gre local $LOCAL_EXT remote $REMOTE_EXT
    ifconfig $tun $LOCAL_IP_TUNNEL pointopoint $REMOTE_IP_TUNNEL
done
GATEWAY=$REMOTE_IP_TUNNEL
route add -net $REMOTE_NET netmask $REMOTE_NETMASK gw $GATEWAY
#route add $REMOTE_NET gw $GATEWAY


## Make an capture traffic: 
##  nc -u 192.168.10.0 4242 < tt
##  tcpdump -s 0 -w multigre_24_afew.pcap
## 
## 
## Some cleaning:
##  for i in `seq 1 24`; do  route del 10.10.$i.10 ; done
##  route del 192.168.10.0
##  for i in `seq 1 24`; do ip tunnel del tun${i}9 ; done

## Trigger kernel panic:
## sh gre_tunnels.sh 37
## ping 192.168.10.1
