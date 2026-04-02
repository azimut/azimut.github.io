#!/bin/bash
set -xe

CERTS='https://android.googlesource.com/platform/system/ca-certificates/+archive/refs/heads/main/files.tar.gz'
mkdir -p new old
if [[ ! -f "new/android-certs.tgz" ]]; then
    cd new/
    wget -O android-certs.tgz "${CERTS}"
    tar xvzf android-certs.tgz
    cd -
fi

DST=/data/local/tmp/certs
adb shell "mkdir -p ${DST}"

idle=1

for filepath in new/*.0; do
    cert="${filepath##*/}"
    pem="${cert%.0}.pem"
    adb push "${filepath}" "${DST}/${pem}"
    mv "${filepath}" old/
    adb shell "am start -n com.android.certinstaller/.CertInstallerMain -a android.intent.action.VIEW -t application/x-x509-ca-cert -d file://${DST}/${pem}"
    sleep "${idle}"
    adb shell input text "${pem}"
    sleep "${idle}"
    adb shell input tap 420 692
    sleep "${idle}"
    sleep "${idle}"
done
