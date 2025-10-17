#!/bin/sh

set -eu

sudo apt update

sudo apt install tlp tlp-rdw powertop mbpfan acpid

sudo systemctl enable mbpfan.service
sudo systemctl daemon-reload
sudo systemctl start mbpfan.service

# Blacklist nouveau
sudo tee /etc/modprobe.d/blacklist-nvidia-nouveau.conf > /dev/null <<'EOF'
blacklist nouveau
blacklist lbm-nouveau
options nouveau modeset=0
alias nouveau off
alias lbm-nouveau off
EOF

# Blacklist WiFi
sudo tee /etc/modprobe.d/blacklist-wifi.conf > /dev/null <<'EOF'
# Disable Broadcom WiFi
blacklist bcma
blacklist bcmwl
blacklist brcmsmac
blacklist brcmfmac
blacklist b43
blacklist wl
blacklist ssb
EOF

# Load i915
echo "i915" | sudo tee -a /etc/initramfs-tools/modules

sudo tee /etc/systemd/system/disable-nvidia-gpu.service > /dev/null <<'EOF'
[Unit]
Description=Remove NVIDIA GPU from PCI bus to save power
After=multi-user.target

[Service]
Type=oneshot
ExecStart=/bin/sh -c 'echo 1 > /sys/bus/pci/devices/0000:01:00.0/remove'
RemainAfterExit=true

[Install]
WantedBy=multi-user.target
EOF

sudo tee /etc/modprobe.d/blacklist-camera.conf > /dev/null <<'EOF'
# Disable FaceTime HD camera and video devices
blacklist facetimehd
blacklist bdc_pci
blacklist uvcvideo
blacklist videobuf2_vmalloc
blacklist videobuf2_memops
blacklist videobuf2_v4l2
blacklist videobuf2_core
blacklist videodev
EOF

sudo tee /etc/systemd/system/disable-camera.service > /dev/null <<'EOF'
[Unit]
Description=Disable FaceTime HD Camera
After=multi-user.target

[Service]
Type=oneshot
ExecStart=/bin/sh -c 'echo 1 > /sys/bus/pci/devices/0000:04:00.0/remove'
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl enable disable-camera.service
sudo systemctl start disable-camera.service

sudo tee /etc/modprobe.d/blacklist-bluetooth.conf > /dev/null <<'EOF'
# Disable all Bluetooth modules
blacklist bluetooth
blacklist btusb
blacklist btrtl
blacklist btbcm
blacklist btintel
blacklist rfcomm
blacklist bnep
blacklist hidp
blacklist hci_uart
blacklist bcm5974 # Trackpad too
EOF

sudo tee /etc/systemd/system/disable-wifi-pci.service > /dev/null <<'EOF'
[Unit]
Description=Disable WiFi PCI Device
After=multi-user.target

[Service]
Type=oneshot
ExecStart=/bin/sh -c 'echo 1 > /sys/bus/pci/devices/0000:03:00.0/remove'
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl enable disable-wifi-pci.service
sudo systemctl start disable-wifi-pci.service

sudo tee /etc/udev/rules.d/81-disable-bluetooth.rules > /dev/null <<'EOF'
# Disable Apple Bluetooth devices
SUBSYSTEM=="usb", ATTR{idVendor}=="05ac", ATTR{product}=="Bluetooth*", ATTR{authorized}="0"
SUBSYSTEM=="usb", ATTR{idVendor}=="05ac", ATTR{bDeviceClass}=="e0", ATTR{authorized}="0"
SUBSYSTEM=="usb", ATTR{idVendor}=="0a5c", ATTR{product}=="*Bluetooth*", ATTR{authorized}="0"

# Generic Bluetooth disable
SUBSYSTEM=="rfkill", ATTR{type}=="bluetooth", ATTR{state}="0"
KERNEL=="hci[0-9]*", SUBSYSTEM=="bluetooth", MODE="0000"
EOF

# sudo nano /etc/default/grub
# GRUB_CMDLINE_LINUX_DEFAULT="... apple_set_os acpi_osi=Darwin i915.modeset=1
# rd.driver.blacklist=nouveau nouveau.modeset=0"
sudo update-initramfs -u
sudo update-grub
sudo reboot

# Clamshell mode
# sudo nano /etc/systemd/logind.conf
# uncomment:
# HandleLidSwitch=ignore
# HandleLidSwitchExternalPower=ignore
# HandleLidSwitchDocked=ignore

sudo tee /usr/local/bin/lid-handler.sh > /dev/null <<'EOF'
#!/bin/sh

if grep -q closed /proc/acpi/button/lid/LID0/state; then
  # Lid closed - turn off display
  echo 0 > /sys/class/backlight/gmux_backlight/brightness
else
  # Lid open - turn on display
  cat /sys/class/backlight/gmux_backlight/max_brightness > /sys/class/backlight/gmux_backlight/brightness
fi
EOF
sudo chmod +x /usr/local/bin/lid-handler.sh

sudo tee /etc/acpi/events/lid > /dev/null <<'EOF'
event=button/lid.*
action=/usr/local/bin/lid-handler.sh
EOF

sudo systemctl enable acpid
sudo systemctl restart acpid

# sudo nano /etc/tlp.conf
# CPU_SCALING_GOVERNOR_ON_AC=performance
