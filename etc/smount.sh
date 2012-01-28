#mkdir /Volumes/shome

error_msg () {
    echo $1
    exit -1
}

if [ -z  "$1" ]
then
    ADDR=sos14.cs.columbia.edu
else
    ADDR=$1
fi

echo "Connecting to " $ADDR

for IMG in tfa tfa++
do
   echo "mounting ${IMG}"
   mkdir /Volumes/${IMG}
   sshfs jikk@${ADDR}:/home/jikk/${IMG} /Volumes/${IMG} -oauto_cache,reconnect,volname=${IMG} || error_msg "can't mount ${IMG}"
done
