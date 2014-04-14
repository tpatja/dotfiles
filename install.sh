#!/bin/sh

function usage {
  echo "USAGE: $1 [-f]"
  echo "  -f disables backup of existing dotfile"
  exit 0
}

function linkfile {
  src=$(readlink -m $1)
  dst=~/.$(echo $1|sed s/^dot//)
  if [ -e $dst ]; then
    if [ "-f" == "$2" ]; then
      rm -f $dst
    else
      echo mv $dst $dst.bak
    fi
  fi
  ln -s $src $dst 
}


if [ "$#" -ne 0 ] && [ "$#" -ne 1 ]; then 
  usage $0
fi

for f in dot*; do
  linkfile $f $1
done
