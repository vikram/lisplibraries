use WWW::Mechanize;
use HTTP::Request;
use HTTP::Response;

my $randomtext = $ARGV[0];

my $host = "localhost:15840";
my $url = "http://${host}";

my $mech = WWW::Mechanize->new( autocheck => 1 );
my $noautomech = WWW::Mechanize->new( autocheck => 0 );

eval {
sub urlexists {
    my($suburl) = @_;

    $mech->get($url . $suburl);
}

sub urlin {
    my($suburl,$regexp) = @_;

    $mech->get($url . $suburl);
    die "$regexp not in ${url}${suburl}. Contents:\n" . $mech->content . "\n" unless $mech->content =~ $regexp;
    die "Random text not found in ${url}${suburl}. Content: ". $mech->content unless $mech->content =~ qr/\|RANDOMID=$randomtext\|/;
}

sub urlnotexist {
    my($suburl) = @_;

    $noautomech->get($url . $suburl);
    die "URL ${url}${suburl} should not exist! It does. Content: " . $noautomech->content if $noautomech->success;
}

sub urlstatus {
    my($suburl,$status) = @_;

    $noautomech->get($url . $suburl);
    my $retstatus = $noautomech->status();
    die "URL ${url}${suburl} returned status code $retstatus, when $status was expected." unless $status == $retstatus;
}


############### TESTS GO HERE #########################

# Test a simple handler
urlin "/handler", qr/simple handler/;

# Test the matching system
# Strict match should match itself, but not more
urlin "/strict-match/", qr/strict matcher/;
urlnotexist "/strict-match/more";

# Loose match should match itself and more
urlin "/loose-match/", qr/loose matcher/;
urlin "/loose-match/more", qr/loose matcher/;

# HTML stream basics
urlin "/html/html-stream", qr[<html>\s*<body>
			      \s*<h1>\s*testing\s*</h1>
			      \s*<p>\s*<a\s+href="/foo.html"\s*>\s*foo\s*</a>\s*</p>
			      \s*<h1>\s*\|RANDOMID=$randomtext\|\s*</h1>
			      \s*</body>\s*</html>]ix;

# Now! With! HTML! Escaping!
# -- araneida escapes using character codes - that's why these things are the way they are
urlin "/html/html-escaped-stream", qr[<html>\s*<body>
				      \s*<h1>\s*testing\s&\#38;\swaiting\s*</h1>
				      \s*<p>\s*<a\s+href="/foo\.html\?a=1&\#38;b=2"\s*>\s*foo\s&\#62;\sbar\s*</a>\s*</p>
				      \s*<h1>\s*\|RANDOMID=$randomtext\|\s*</h1>
				      \s*</body>\s*</html>]ix;

# link functionality
urlin "/url/link", qr/\?slays=everything%20but%20squid&ow=yes/;
urlin "/url/link?and=this", qr/\?slays=everything%20but%20squid&ow=yes&and=this/;

# test a dispatching handler's base response
urlin "/dispatch-handler/", qr/dispatch handler/;
urlin "/dispatch-handler/subhandler", qr/dispatch subhandler handler/;

# test error responses
urlstatus "/error/bad-request", 400;
urlstatus "/error/unauthorized", 401;
urlstatus "/error/payment-required", 402;
urlstatus "/error/forbidden", 403;
urlstatus "/error/not-found", 404;
urlstatus "/error/method-not-allowed", 405;
urlstatus "/error/not-acceptable", 406;
urlstatus "/error/proxy-authentication-required", 407;
urlstatus "/error/request-time-out", 408;
urlstatus "/error/conflict", 409;
urlstatus "/error/gone", 410;
urlstatus "/error/length-required", 411;
urlstatus "/error/precondition-failed", 412;
urlstatus "/error/request-entity-too-large", 413;
urlstatus "/error/request-url-too-large", 414;
urlstatus "/error/unsupported-media-type", 415;
urlstatus "/error/internal-server-error", 500;
urlstatus "/error/not-implemented", 501;
urlstatus "/error/bad-gateway", 502;
urlstatus "/error/service-unavailable", 503;
urlstatus "/error/gateway-time-out", 504;
urlstatus "/error/version-not-supported", 505;

# Test request-redirect
# explanation: we go to the first and are redirected to the second. We therefore should get the text
# of the second.
urlin "/redirect/first", qr/redirect second handler/;
urlin "/redirect-handler/first", qr/redirect-handler second handler/;

# Test cookie request
# Cookies are tested twice: once to see if the client gets them correctly
# and once again on the server side to make the server gets them correctly
# there's a redirect from receive to send, so make sure of that then check cookies
urlin "/cookie/send", qr/cookie receive handler/;

my $tmp = sub { urlin "/cookie/receive", @_ };
$tmp->(qr/unsafecookie simplecookie=simple-value/);
$tmp->(qr/safecookie simplecookie=simple-value/);
$tmp->(qr/unsafecookie ladencookie=123ladenvalue123---/);
$tmp->(qr/safecookie ladencookie=123ladenvalue123---/);

# now check the local side
my %cookies_found = {};
$mech->cookie_jar->scan(sub { my($ver,$key,$val) = @_; $cookies_found{$key} = $val; });
sub cookie_check {
    my($key,$val) = @_;
    exists $cookies_found{$key} && $cookies_found{$key} == $val or die "Cookie $key=$val not found"
    }
cookie_check "simplecookie", "simple-value";
cookie_check "ladencookie", "123ladenvalue123---";


# Test conditional get
urlin "/conditional-get", qr/handler/;
my $request = HTTP::Request->new('GET', $url . "/conditional-get");
$request->header('If-Modified-Since' => HTTP::Date::time2str());
my $response = $noautomech->request($request);
die "Conditional get failed with ${url}/conditional-get . Returned code " . $response->code unless $response->code == 304;

# Test parameters
sub param_check {
    urlin "/paramtest?foo=bar", @_;
}
param_check qr/foo-alist bar/;
param_check qr/foo untaint case bar/;
param_check qr/foo untaint nocase bar/;
param_check qr/foo taint case bar/;
param_check qr/foo taint nocase bar/;
param_check qr/foo type taint case FUNCTION/;
param_check qr/foo type taint nocase FUNCTION/;

# Test with-params functionality
urlin "/with-paramtest?foo=bim;bar=baz", qr/foo: bim/;
urlin "/with-paramtest?foo=bim;bar=baz", qr/bar: baz/;
urlin "/with-paramtest?foo=bim", qr/foo: bim/;
urlin "/with-paramtest?foo=bim", qr/bar: nil/;

# Test with-tainted-params functionality
urlin "/with-tainted-paramtest?foo=bim;bar=baz", qr/taint-foo: bim/;
urlin "/with-tainted-paramtest?foo=bim;bar=baz", qr/taint-bar: baz/;
urlin "/with-tainted-paramtest?foo=bim", qr/taint-foo: bim/;
urlin "/with-tainted-paramtest?foo=bim", qr/taint-bar: nil/;

# Test alternate MIME types
{
    my $response = $mech->get($url."/mime/text-plain");    
    die "Expected MIME type text/plain, got MIME type ${response->content_type} for ${url}/mime/text-plain"
	unless $response->content_type == "text/plain";
}

# Test authentication
urlstatus "/authenticated/test", 401;
$mech->credentials($host,"testrealm","testuser","testpass");
urlin "/authenticated/test", qr/authenticated test handler/;

# Test authorization
urlstatus "/authorization/test", 401;
$mech->add_header( Referer => "http://example.com/foobar");
urlin "/authorization/test", qr/authorization test handler/;

# Test attach-hierarchy
urlin "/hier/foo", qr/hier foo handler/;
urlin "/hier/bar", qr!hier bar handler: \Q$url\E/hier/bar\.!;

# Test advanced attach-hierarchy
urlin "/hier-advanced/foo", qr/hier-advanced foo handler: mooofooo/;
urlin "/hier-advanced/bar", qr/hier-advanced bar handler/;
urlin "/hier-advanced/yes", qr!hieradv xbase mim = yes - \Q$url\E/hier-advanced/\.!;
urlin "/hier-advanced/no", qr!hieradv xbase mim = no - \Q$url\E/hier-advanced/\.!;
urlin "/hier-advanced/maybe", qr!hieradv xbase mim = maybe - \Q$url\E/hier-advanced/\.!;
urlin "/hier-advanced/so", qr!hieradv xbase mim = so - \Q$url\E/hier-advanced/\.!;

# Test basic parametermethods
urlin "/parametermethod/basic", qr/Hello World!/;
urlin "/parametermethod/basic?greeting=go+away", qr/go away/;

# Test basic tainted parametermethods
urlin "/parametermethod/taintedbasic", qr/Hello World!/;
urlin "/parametermethod/taintedbasic?greeting=go+away", qr/go away/;

# Test basic parametermethods with function parameters
urlin "/parametermethod/funcparams1", qr/one Hello World!/;
urlin "/parametermethod/funcparams1?greeting=go+away", qr/one go away/;
urlin "/parametermethod/funcparams2", qr/two Hello World!/;
urlin "/parametermethod/funcparams2?greeting=go+away", qr/two go away/;

# Test basic specialized handlers
urlin "/parametermethod/specialize", qr/Hello yourself!/;
urlin "/parametermethod/specialize?message=go+away", qr/go away/;
urlin "/parametermethod/specialize?message=goodbye", qr/The goodbye message!/;

# Test no parameters
urlin "/parametermethod/noparams?foo=foo", qr/noparam Foo provided/;
urlin "/parametermethod/noparams", qr/noparam No parameters given/;


# Test # key precendence
urlin "/parametermethod/test1", qr/test1 You should see this. Foo: foo Bar: bar/;
urlin "/parametermethod/test1?foo=bim&bar=baz", qr/test1 You should see this. Foo: bim Bar: baz/;

# Test # specialized requires precedence
urlin "/parametermethod/test2?foo=foo&bar=baz", qr/test2 You should see this. Foo: foo Bar: baz/;
urlin "/parametermethod/test2?foo=bim&bar=baz", qr/test2 When foo is "foo" you should NOT see this. Foo: bim Bar: baz/;

# Test # specialized requires precedence
urlin "/parametermethod/test3?foo=foo&bar=baz", qr/test3 You should see this. Foo: foo Bar: baz/;
urlin "/parametermethod/test3?foo=bim", qr/test3 When bar exists you should NOT see this. Foo: bim Bar: bozo/;

# Test parameter order
urlin "/parametermethod/paramorder", qr/paramorder a: thedefault-a b: thedefault-b c: thedefault-c/;
urlin "/parametermethod/paramorder?a=1&b=2&c=3", qr/paramorder a: 1 b: 2 c: 3/;

# Test tainted and untainted key value defaults
urlin "/parametermethod/defaultkeyvalues?r=yes", qr/defaultkeyvalues r: yes a: thedefault-a b: thedefault-b c: thedefault-c/;
urlin "/parametermethod/defaultkeyvalues-tainted?r=yes", qr/defaultkeyvalues-tainted r: yes a: thedefault-a b: thedefault-b c: thedefault-c/;

# Test that conditions are thrown
urlin "/parametermethod/conditioncheck", qr/all is well/;

# Test template usage
urlin "/template", qr{<h1><span>contents</span>\s*</h1>};

# Test HTML macros
urlin "/html/tag-basic", qr{Mug of joe\? c\|_\|};
urlin "/html/tag-advanced", qr{<ul>\s*<li class="odd">odd</li>\s*<li class="even">even</li>\s*<li class="odd">odd</li>\s*<li class="even">even</li>\s*</ul>}i;

# Quit
urlin "/quit", qr/quitting/;
};
if ($@) {
print "Failure: $@\n";
print "Quitting.\n";
$noautomech->get($url."/quit");
exit 1;
}

#############################################
print "All tests passed\n";
exit 0;
