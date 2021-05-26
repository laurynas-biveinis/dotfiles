#!/bin/sh

sudo sh -c "echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo"
sudo sh -c 'echo 0 > /proc/sys/kernel/randomize_va_space'
# teststar: disable hyperthreading
sudo sh -c "echo 0 > /sys/devices/system/cpu/cpu4/online"
sudo sh -c "echo 0 > /sys/devices/system/cpu/cpu5/online"
sudo sh -c "echo 0 > /sys/devices/system/cpu/cpu6/online"
sudo sh -c "echo 0 > /sys/devices/system/cpu/cpu7/online"
