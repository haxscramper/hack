echo -ne "\0\0\x4\0\0\0\0\0" > /dev/hidg0 #press the A-button
sleep 1
echo -ne "\0\0\0\0\0\0\0\0" > /dev/hidg0 #release all keys

