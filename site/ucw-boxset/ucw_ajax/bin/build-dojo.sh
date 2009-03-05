
UCW_HOME="`dirname $0`/.."
DOJO_HOME="`dirname $0`/../../dojo"

if [ "$1" != "" ]; then
    DOJO_HOME=$1
fi

if [ ! -e "$DOJO_HOME" ]; then
    echo $DOJO_HOME does not exists
    exit 1
fi

cd "$DOJO_HOME"
DOJO_HOME=`pwd`

cd - >/dev/null

cd "$UCW_HOME"
UCW_HOME=`pwd`

echo Assuming dojo home is $DOJO_HOME, ucw home is $UCW_HOME

cd "$DOJO_HOME/buildscripts"
cp "$UCW_HOME/etc/ucw.profile.js" "$DOJO_HOME/buildscripts/profiles/ucw.profile.js"
ant -Dprofile="ucw" -Ddocless=true clean release intern-strings

cp "$DOJO_HOME/release/dojo/dojo.js" "$UCW_HOME/wwwroot/dojo/"
rm -rf "$UCW_HOME/wwwroot/dojo/src/"
cp -r "$DOJO_HOME/release/dojo/src/" "$UCW_HOME/wwwroot/dojo/"


