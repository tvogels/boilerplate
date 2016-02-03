for FILE in `ls *.html`
do
  ENCODING=`uchardet $FILE`
  echo $FILE: $ENCODING
  ENC=`echo $ENCODING | sed 's#^x-mac-#mac#'`
  if [ "$ENC" = "ascii/unknown" ];
  then
    iconv -f ascii -t utf-8 $FILE > $FILE.utf8
  else
    iconv -f $ENC -t utf-8 $FILE > $FILE.utf8
  fi
done