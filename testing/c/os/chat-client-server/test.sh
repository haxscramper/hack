#!/usr/bin/env bash
set -o nounset
set -o errexit

clang++ -g server.cpp -o server
clang++ -g client.cpp -o client

if [ "${1:-'qqq'}" == "compile" ]; then
    echo "only running compilation"
    exit 0
fi

echo -e "u1 p1\nu2 p2" > user-list.tmp

fulltest="false"

if [ "$fulltest" == "true" ]; then
  ./server &

  sleep 1

  cat << CLIENT1 > /tmp/client-1.sh
#!/usr/bin/env bash
cat << EOF | $PWD/client
/login u1 p1
/connect u2
First message
Second message
More messages
EOF
CLIENT1

  chmod +x /tmp/client-1.sh
  kitty @ new-window --title "Client 1" --keep-focus
  kitty @ launch --match "title:Client 1" /tmp/client-1.sh

  sleep 2

  cat << CLIENT2 > /tmp/client-2.sh
#!/usr/bin/env bash
cat << EOF | $PWD/client
/login u2 p2
/connect u1
Send reply
Send another reply
/server-shutdown
EOF
CLIENT2
  
  chmod +x /tmp/client-2.sh
  kitty @ new-window --title "Client 2" --keep-focus
  kitty @ launch --match "title:Client 2" /tmp/client-2.sh

  sleep 10

  kitty @ close-window --match "title:Client 1"
  kitty @ close-window --match "title:Client 2"
elif [ "$fulltest" == "login" ]; then
    ./server &

    sleep 1

    cat << EOF | while read -r line; do echo $line; sleep 0.1; done | ./client &
/login u1 p1
/server-shutdown
EOF

else
  ./server &

  sleep 1

  cat << EOF | while read -r line; do echo $line; sleep 0.1; done | ./client &
/login u1 p121
/login u1 p1
/connect u2
First message
Second message
More messages
EOF

  sleep 2

  cat << EOF | while read -r line; do echo $line; sleep 0.1; done | ./client &
/login u2 p2
/connect u1
Send reply
Send another reply
/server-shutdown
EOF
fi
