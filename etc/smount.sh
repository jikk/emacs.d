#mkdir /Volumes/shome

error_msg () {
    echo $1
    exit -1
}

if [ -z  "$1" ]
then
    ADDR=sos15.cs.columbia.edu
else
    ADDR=$1
fi

echo "Connecting to " $ADDR


#for IMG in tfa-parallel tfa++ libdft-ng doc
for IMG in tfa-parallel doc tfa
do
   #CHK_MNT=`df |grep ${IMG} | wc -l`
    CHK_MNT=0
   if [ $CHK_MNT == 0 ] ; then
       echo "mounting ${IMG}"
       mkdir /Volumes/${IMG}
       sshfs jikk@${ADDR}:/home/jikk/${IMG} /Volumes/${IMG} -oauto_cache,reconnect,volname=${IMG} || error_msg "can't mount ${IMG}"
    else
       echo "not mounting ${IMG}"
    fi
done
