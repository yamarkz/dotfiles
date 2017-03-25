#! /usr/bin/env zsh

loading() {
  local count=30
  if [ $# -eq 1 ]; then
    count=$1
  fi

  for i in `seq 1 1 $count`
  do
    echo -en '|\b'  1>&2; sleep 0.05;
    echo -en '/\b'  1>&2; sleep 0.05;
    echo -en '-\b'  1>&2; sleep 0.05;
    echo -en '\\\b' 1>&2; sleep 0.05;
  done
  # 最後の出力を消す
  echo -en ' \b' 1>&2;
  exit 0
}

loading $@
