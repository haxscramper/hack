#! /bin/bash
# /etc/init.d/init_keyboard.sh

### BEGIN INIT INFO
# Provides:          init_keyboard.sh 
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start daemon at boot time
# Description:       Enable service provided by daemon.
### END INIT INFO

echo "Started keyboard" > /tmp/init_keyboard_report
cd /home/pi/Code
sudo python main_control.py
echo "Finished keyboard" >> /tmp/init_keyboard_report
