<?php
  require 'vrmlengine_functions.php';
  common_header('KambiScript', LANG_EN, 'KambiScript language.');
?>

<?php echo pretty_heading('KambiScript language');  ?>

<p><tt>KambiScript</tt> is a simple scripting language used in
our Kambi VRML game engine. You can use it in VRML/X3D <tt>Script</tt>
nodes. Also it's syntax of mathematical expressions is usable
throughout our engine, for example <?php echo a_href_page(
'glplotter and gen_function',
'glplotter_and_gen_function'); ?> (which are completely
not related to VRML) use this syntax to define function expressions.</p>

<p>The language is deliberately very simple. It's a scripting language,
with features inspired by many other languages (and by programmer's laziness,
for example I was too lazy to construct grammar with things like "if",
so instead <tt>if</tt> and similar constructs is just written like
a normal function).</p>

<p>Please note that
<b>I do not advice VRML authors to use this language</b>,
as it's specific to our engine, and probably always will be.
You should rather use ECMAScript
for compatibility &mdash; but for now ECMAScript is not implemented yet...
The advantage of KambiScript is that it's built-in and integrated
into our engine, without requiring any external libraries. It may also
serve as a testbed for some of my ideas.</p>

<?php
  $toc = new TableOfContents(
    array(
      new TocItem('Placing scripts inside VRML Script URLs', 'script_urls'),
      new TocItem('Syntax', 'syntax'),
      new TocItem('Examples', 'examples'),
      new TocItem('Built-in functions', 'built_in_functions'),
      new TocItem('Precise grammar', 'precise_grammar')
    )
  );
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>URLs in <tt>Script</tt> node starting with <tt>kambiscript:</tt>
are understood to contain program in KambiScript language.
URLs to external files with extension <tt>.kscript</tt> point
to whole files in KambiScript language. Like</p>

<pre class="light_bg">
Script {
  inputOnly SFFloat foo
  outputOnly SFFloat foo_plus_one

  url "kambiscript:
function foo(value, timestamp)
foo_plus_one := value + 1
"
}

Script {
  url "my_script.kscript"
}
</pre>

<?php echo $toc->html_section(); ?>

<p><i>Program</i> is just a set of functions. VRML engine will take care
to call function <tt>xxx</tt> when input event of the same name will arrive.</p>

<p><i>Syntax is free-form</i>, the only use of whitespace (including newlines,
or any indentation) is to separate tokens when needed (for example, between
two identifiers).</p>

<p><i>Types</i> are never explicitly declared, and are checked
at runtime. Internally, all types available in VRML scripts are
the VRML field's types (integers, floats, vec2f, vec3f, vec4f, strings,
images (SFImage) and such, and arrays of them). For uses of KambiScript
for mathematical expressions (like in <tt>glplotter</tt>), all calculations
use internally a floating-point type with best precision on given platform.
</p>

<p><i>Expressions and instructions</i> are the same thing within
the language. For example, "assignment" is an instruction, since
it causes calculation of the right operand and assigning it to the left
operand, but it's also an "expression", since it returns the value
(the assigned value).
So "chained" assignment, like in C, is possible (although
usually discouraged, to not obfuscate the code): <tt>a := b := x + y</tt> works.
In the rest of this description, terms "instruction" and "expression"
mean exactly the same thing.</p>

<p><i>Function</i> starts from the <tt>function</tt> keyword,
then function name (identifier),
then list of 0 or more parameters (identifiers separated by commas),
always within parenthesis. For functions within VRML Script nodes:
<tt>initialize</tt> and <tt>shutdown</tt> must take exactly
one parameter (timestamp of the event: SFTime), functions called
by incoming events must take exactly two parameters (value send to the event,
and timestamp: SFTime).</p>

<p>Function body is just a sequence of expressions separated by
semicolon. Formally, function body is actually a single expression,
but we have a semicolon operator: <tt>A;B</tt> means "calculate A,
ignore result, then calculate and return result of B".
For now, result of functions body is ignored (so all our functions
are in fact <i>procedures</i>).
Semicolon works like a delimiter (not a terminator,
so it's required only between instructions).</p>

<p>An <i>assignment instruction</i>:
Left operand is the name of output
event or a field (exposed or not) to assign/send a value.
Then we have the assignment operator <tt>:=</tt> (Pascal-like).
And the right operand, which is an expression that calculates value to assign.
</p>

<ul>
  <li><p>Assigning value to initializeOnly (not exposed) field is simple, it just
    assigns value to this field. You can use initializeOnly fields as
    "variables" available for your scripts (since KambiScript doesn't
    allow you to declare or use new variables within the program, for now).
    </p></li>

  <li><p>Assigning value to output event results in sending this event,
    assigning to exposed field results in sending this to input event of this field.

    <p>Following ECMAScript standard, events are not send immediately
    (right at the assignment), instead they are stacked and send
    when script function finished execution. When you assigned
    multiple values for the same field, only the last one is send.
    In case of multiple-value fields, the combined end value is send.
    For example, assuming <tt>output</tt> is an <tt>outputOnly</tt>
    event of MFFloat type:

<pre class="light_bg">
  function foo(value, timestamp)
  output := array(0, 1, 2, 3);
  output[1] := 666;
  output[2] := 44
</pre>

    The example above will send one <tt>output</tt> event with value
    <tt>(0, 666, 44, 3)</tt>. In other words, this works according to
    X3D specification for ECMAScript bindings.
    </p></li>
</ul>

<p>Right side of the assignment instruction is the value to calculate
and assign. In short, a normal mathematical expression is allowed there,
just like you seen in all programming languages. We have multiplicative
operators (<tt>/, *, ^, %</tt>),
we have additive operators (<tt>+, -</tt>) with lower
priority, we have comparison operators
(<tt>&lt;, &gt;, &lt;=, &gt;=, = or &lt;&gt;</tt>) with even lower
priority (for float expressions, like the one for glplotter, relative
operators result in 0 (false) or 1 (true)). We have all standard math
functions built-in. See detailed grammar below for a full list.

<p>For VRML scripts, you can reference field's within nodes by qualifying
<tt>node_name.field_name</tt>. For multiple-value fields, you can use
an array index, like <tt>field_name[integer index]</tt>.

<?php echo $toc->html_section(); ?>

<p>Some examples of simple mathematical expressions for glplotter:</p>

<pre class="light_bg">
  sin(x) ^ 10
  2 * (cos(ln(x)) - 1)
  sin(x) &gt; cos(x)
  or( sin(x) &gt; cos(x), sin(x) &gt; 0 )
</pre>

<p>Some example of simple program for VRML script node:</p>

<pre class="light_bg">
Script {
  # Let's assume some TouchSensor.touchTime is routed here.
  # When user clicks on this touch sensor, you want to close the door
  # if they are open, or open them if they are closed.
  inputOnly SFTime touch_time

  initializeOnly SFBool open FALSE

  # Let's assume this is routed to some TimeSensor.set_startTime
  # that starts closing animation.
  outputOnly SFTime close_time

  # Let's assume this is routed to some TimeSensor.set_startTime
  # that starts opening animation.
  outputOnly SFTime open_time

  url "kambiscript:
function touch_time(value, timestamp)
if (open,
    close_time := timestamp,
    open_time := timestamp);
open := Not(open)
"
}
</pre>

<p>Example script behavior above could also be done by combining
<tt>BooleanToggle</tt>, <tt>BooleanFilter</tt>, <tt>TimeTrigger</tt>
X3D nodes.
But script is already simpler and shorter, and allows you to trivially
add other interesting things.</p>

<pre class="light_bg">
# Simple converter from SFString to MFString using built-in array() function.
Script {
  inputOnly SFString input
  outputOnly MFString output
    url "kambiscript:
function input(value, timestamp)
output := array(value)
"
}
</pre>

<?php echo $toc->html_section(); ?>

<ul>
  <li><tt>Sin</tt>, <tt>Cos</tt>, <tt>Tan</tt>, <tt>CoTan</tt>
  <li><tt>ArcSin</tt>, <tt>ArcCos</tt>, <tt>ArcTan</tt>, <tt>ArcCoTan</tt>
  <li><tt>SinH</tt>, <tt>CosH</tt>, <tt>TanH</tt>, <tt>CoTanH</tt>
  <li><tt>Log2</tt>, <tt>Ln</tt>, <tt>Log</tt>, <tt>Power2</tt>,
    <tt>Exp</tt>, <tt>Power</tt>, <tt>Sqr</tt>, <tt>Sqrt</tt><br>
    (<tt>Log2(x) = Log(2, x)</tt>,
     <tt>Power2(x) = Power(2, x) = 2^x</tt>,
     <tt>Exp(x) = Power(enat, x) = enat^x</tt>)
  <li><tt>Sgn</tt>, <tt>Abs</tt>, <tt>Ceil</tt>, <tt>Floor</tt>
  <li><tt>Greater</tt>, <tt>Lesser</tt>, <tt>GreaterEq</tt>,
    <tt>LesserEq</tt>, <tt>Equal</tt>, <tt>NotEqual</tt><br>
    (when these are forced to return numbers, they return 0 (false) or 1 (truth))
  <li><tt>Or</tt>, <tt>And</tt>, <tt>Not</tt><br>
    (when these are forced to return numbers, they return 0 (false) or 1 (truth).
    Also, when numbers are arguments, 0 is interpreted as false,
    and anything else is true.)
  <li><tt>Array</tt> &mdash; constructs an array for multiple-value VRML field.
</ul>

<?php echo $toc->html_section(); ?>

<pre class="light_bg">
  Operand (aka "something that can be assigned") =
    Identifier [{"." Identifier}] ["[" Expression "]"]

  Factor = Operand |
           Constant |
           "-" Factor |
           "(" Expression ")" |
           FunctionName [ "(" Expression [{"," Expression}] ")" ]
  FactorOperator = "^" | "*" | "/" | "%"

  # In other words, all multiplicative operators have the same priority
  # and are left-associative.
  # "^" operator is for power.
  #     X ^ Y = Power(X, Y)
  #     This works for non-integer Y, but in this case Y has to be &gt;= 0.
  # "%" operator is for modulo (remainder of division).
  #     X % Y = X - Floor(X/Y) * Y

  Term = Factor [{FactorOperator Factor}]
  TermOperator = "+" | "-"

  ComparisonArgument = Term [{TermOperator Term}]
  ComparisonOperator = "&lt;" | "&gt;" | "&lt;=" | "&gt;=" | "=" | "&lt;&gt;"

  NonAssignmentExpression = ComparisonArgument [{ComparisonOperator ComparisonArgument}] |

  # Programmers using our engine: note that KambiScriptParser.ParseFloatExpression
  # parses exactly "NonAssignmentExpression" token, as defined above,
  # with the Factor definition hacked to also allow only NonAssignmentExpression
  # inside parenthesis. In other words, ParseFloatExpression takes care to only
  # parse a calculated expression, without any assignments or sequence.

  PossiblyAssignmentExpression = NonAssignmentExpression |
                                 Operand ":=" PossiblyAssignmentExpression

  Expression = PossiblyAssignmentExpression [{";" PossiblyAssignmentExpression}] |

  Function = "function" "(" [Identifier [{"," Identifier}] ")" Expression
  Program = [{Function}]

  # Programmers using our engine: note that KambiScriptParser.ParseProgram
  # parses exactly "Program" token defined above.

  # ------------------------------------------------
  # Programmers using our engine: note that above part of the grammar
  # was handled inside KambiScriptParser. Grammar below is handled
  # inside KambiScriptLexer.
  # A "token" returned by KambiScriptLexer corresponds to a non-terminal
  # symbol in the part of the grammar below, resolved by lexer.

  # Identifier is just a sequence of letters, underscores, digits,
  # not starting with a digit.
  Identifier = Letter [{Letter | Digit}]
  Letter = 'a' .. 'z' | 'A' .. 'Z' | "_"
  Digit = '0' .. '9'

  Constant = "pi" | "enat" | Digit [{Digit}] ["." Digit [{Digit}] ]

  FunctionName = (see list of built-in functions above)
</pre>

<p>Generally, at least one whitespace is required between two tokens.
But there are many exceptions, when situation is unambiguous,
for example no whitespace is needed between
identifiers and parenthesis.
In case of uncertainty lexer is greedy, that is lexer tries to eat as much
characters as it can for a single token.</p>

<p>Case sensitivity: built-in constants and function names
are not case-sensitive. Also <tt>glplotter</tt> and everything
else that uses only a subset of KambiScript to get function expressions
is not case-sensitive. However, <i>whole VRML/X3D is generally
case-sensitive</i>, so the names of fields/events for VRML/X3D is
case-sensitive.

<!--
(wszystko jest na wartosciach rzeczywistych;
pamietaj tez ze operator potegowania ma taki sam priorytet jak
np. mnozenie a operatory o rownym priorytecie sa obliczane od
lewej do prawej wiec np. 2*4^2 = 8^2, nie 2*16))
-->

<?php
  if (!IS_GEN_LOCAL) {
    php_counter("kambi_script", TRUE);
  };

  common_footer();
?>
