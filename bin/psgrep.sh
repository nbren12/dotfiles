#!/bin/bash

ps aux | grep $1 | awk -F' ' '!/grep/{print $2}'
