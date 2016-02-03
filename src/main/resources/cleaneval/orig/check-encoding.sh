for FILE in `ls *.utf8`
do
  ENCODING=`uchardet $FILE`
  echo $FILE: $ENCODING
done