<?php
  require 'vrmlengine_functions.php';
  common_header('KambiScript language', LANG_EN);

  function script_func($name, $title)
  {
    echo '<a href="#function_' . $name . '"><tt>' . $title . '</tt></a>';
  }
?>

<?php echo pretty_heading('KambiScript language<br/>
<span style="font-size: medium">Simple scripting language for Kambi VRML game engine</span>'); ?>

<p><tt>KambiScript</tt> is a simple scripting language used in
our <i>Kambi VRML game engine</i>. You can use it in VRML/X3D <tt>Script</tt>
nodes. Also it's syntax of mathematical expressions is usable
throughout our engine, for example <?php echo a_href_page(
'glplotter and gen_function',
'glplotter_and_gen_function'); ?> (which are completely
not related to VRML) use this syntax to define function expressions.</p>

<p>The language is deliberately very simple. It's a scripting language,
with features inspired by many other languages, and by author's laziness.
For example I was too lazy to add <tt>if</tt>, <tt>while</tt> and such
constructs to the grammar,
instead you have built-in functions like
<tt>if(condition, then_code, else_code)</tt>.
<b>This language doesn't try to compete with other scripting languages</b>
(like <i>ECMAScript</i>, commonly used in VRML scripting).
It's not suitable for larger programs
(for starters, you cannot even define your own types).<!-- ; you also cannot
currently call user-defined functions (you can only call built-in functions;
user-defined functions are only automatically called when VRML Script
receives an event); also you cannot declare your own local variables
(but you can use VRML Script initializeOnly fields for this purpose)-->
Also it's specific to our engine, and probably always will be.</p>

<p>That said, the language is powerful enough for many uses.
You can process all VRML field types with it, including strings,
vectors, matrices and even images. Also arrays (VRML MFXxx fields) are covered.
There are many built-in functions and operators, heavily overloaded
for all types where they apply (for example, you can add numbers,
vectors, matrices or strings).
It's an integral part of our engine, without the need for any external libraries.
And do note that our engine doesn't support (yet) ECMAScript for VRML script
at all, so this is the only way to do scripting for now (without
writing and compiling any ObjectPascal code).</p>

<p>Programmers may also be interested that language implementation is flexible,
you can extend it easily from ObjectPascal (adding new data-types and
built-in functions), for many uses (not necessarily related with VRML).
The language is completely safe (that is, there's no possibility for
malicious script author to do something harmful)
simply because the language is a closed data-processing language
(there are absolutely no I/O routines or anything like this).
</p>

<?php
  $toc = new TableOfContents(
    array(
      new TocItem('Writing scripts inside VRML Script URLs', 'script_urls'),
      new TocItem('Examples', 'examples'),
      new TocItem('Syntax', 'syntax'),
      new TocItem('Types and constants', 'types_constants', 1),
      new TocItem('Programs and expressions', 'programs_expressions', 1),
      new TocItem('Built-in functions', 'built_in_functions'),
      new TocItem('Precise grammar', 'precise_grammar')
    )
  );
  $toc->echo_numbers = true;
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
open := not(open)
"
}
</pre>

<p>Example script behavior above could also be done by combining
<tt>BooleanToggle</tt>, <tt>BooleanFilter</tt>, <tt>TimeTrigger</tt>
X3D nodes.
But script is already simpler and shorter, and allows you to trivially
add other interesting things.</p>

<pre class="light_bg">
# Simple converter from SFString to MFString using built-in <?php script_func('array', 'array'); ?> function.
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

<p><i>Syntax is free-form</i>, the only use of whitespace (including newlines,
or any indentation) is to separate tokens when needed (for example, between
two identifiers).</p>

<?php echo $toc->html_section(); ?>

<p><i>Types</i> are never explicitly declared, and are checked
at runtime. Four core types are available:</p>

<ol>
  <li><p>Integers. (32-bit for now, maybe will be extended to 64-bit
    if the need will arise.) Syntax of integer constants is obvious,
    like <tt>123</tt>. Built-in function
    <?php script_func('int', 'int(...)'); ?> allows
    you to convert other core types into integer.</p></li>

  <li><p>Floats. (Uses the best floating-point type precision on given
    platform, which means at least Double, but on many platforms Extended.)
    Syntax of float constants
    is also obvious, like <tt>3.1415</tt>. You have also
    constants <tt>pi</tt> and <tt>enat</tt> (Euler's number).
    Built-in function
    <?php script_func('float', 'float(...)'); ?> allows
    you to convert other core types into float.</p></li>

  <li><p>Booleans. Two obvious constants are available, <tt>false</tt>
    and <tt>true</tt> (case is ignored, as usual in KambiScript,
    so you can also write uppercase
    <tt>FALSE</tt> or <tt>TRUE</tt> like in classic VRML).
    Built-in function
    <?php script_func('bool', 'bool(...)'); ?> allows
    you to convert other core types into boolean.</p></li>

  <li><p>Strings. Syntax of constants is Pascalish (in apostrophes, and two
    consecutive apostrophes inside mean that you want a single literal
    apostrophe character). For example <tt>'He said "It''s mine."'</tt>.
    Built-in function
    <?php script_func('string', 'string(...)'); ?> allows
    you to convert other core types into string.</p></li>
</ol>

<p>The one and only implicit type conversion (promotion) of types is from
integer to float (for example, <tt>my_float := 44</tt> works,
you don't have to write <tt>my_float := 44.0</tt>).
In particular, note that <i>boolean type is not interchangeable
with integer</i> like in C. If you want to convert between boolean and integer,
you have to convert explicitly by <tt>bool(my_int)</tt> or <tt>int(my_bool)</tt>,
like in Pascal. The only exception is when using KambiScript solely for
mathematical expressions (like in <?php echo a_href_page(
'glplotter and gen_function',
'glplotter_and_gen_function'); ?>, internally using <tt>ParseFloatExpression</tt>
function): in this case, result is always implicitly converted to float,
like it would be embedded within <tt>float(...)</tt> call.
</p>

<p>When using KambiScript inside VRML scripts, internally you have
all the VRML field types available (which means that
vec2f, vec3f, vec4f, matrix, image and others are included).
There is no special syntax for reading/writing other types, instead
you have many functions to construct and set them.
For example for vec3f type you have "constructor"
<?php script_func('vector', 'vector(x, y, z)'); ?> ,
reader for particular component <?php script_func('vector_get', 'vector_get(vector, index)'); ?>,
and setter for particular component <?php script_func('vector_set', 'vector_set(vector, index, component_value)'); ?>.
Even images have functions to create and modify them, which means
that you can use KambiScript to perform basic image creation and processing.</p>

<p>Also array types are internally available, for VRML multiple-value
(MFXxx) types. Again no special syntax is available (sorry, no bracket parenthesis),
but there are functions to construct array
<?php script_func('array', 'array(item1, item2, ...)'); ?>,
read component <?php script_func('array_get', 'array_get(array, index)'); ?> and
set component <?php script_func('array_set', 'array_set(array, index, component_value)'); ?>.

<?php echo $toc->html_section(); ?>

<p><i>Program</i> is just a set of functions. VRML engine will take care
to call function <tt>xxx</tt> when input event of the same name will arrive.</p>

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
so it's used only between instructions).</p>

<p>Note that the way semicolon and expressions are defined means
that we don't need any special syntax for compound instruction
(like <tt>begin end</tt> in Pascal or
<tt>{ }</tt> in C). Instead, normal parenthesis may be
used if necessary to group instructions.</p>

<p>An <i>assignment instruction</i> is an operand, followed by
the assignment operator <tt>:=</tt> (Pascal-like),
followed by an expression to calculate value to assign.

<p>For VRML scripts, you are allowed to assign to output events
and to fields (exposed or not). Events sending behavior follows ECMAScript
standard:</p>

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
    multiple values for the same field/event, only the last one is send.
    In case of multiple-value fields, the combined end value is send.
    For example, assuming <tt>output</tt> is an <tt>outputOnly</tt>
    event of MFFloat type:

<pre class="light_bg">
function foo(value, timestamp)
  output := array(0.0, 1.0, 2.0, 3.0);
  array_set(output, 1, 666.0);
  array_set(output, 2, 44.0)
</pre>

    <p>The example above will send one <tt>output</tt> event with value
    <tt>(0.0, 666.0, 44.0, 3.0)</tt>.</p></li>
</ul>

<p>Right side of the assignment instruction is the value to calculate
and assign. In short, a normal mathematical expression is allowed there,
just like you seen in all programming languages. We have multiplicative
operators (<tt>/, *, ^, %</tt>),
we have additive operators (<tt>+, -</tt>) with lower
priority, we have comparison operators
(<tt>&lt;, &gt;, &lt;=, &gt;=, = or &lt;&gt;</tt>) with even lower
priority. We have all standard math
functions. Built-in functions and operators are overloaded
for all suitable types. Section below gives a full list of operators
and functions.</p>

<?php echo $toc->html_section(); ?>

<p>Type conversion:

<ul>
  <li><p><a name="#function_int"><tt>int(...)</tt></a> converts a "core" type
    to an integer.</p>

    <p>Float is converted to int by discarding it's fractional
    part (like in C; for positive numbers, this is like <tt>floor</tt>, for negative
    this is like <tt>ceil</tt>).
    There are also functions <tt>floor</tt>, <tt>ceil</tt> and
    <tt>round</tt> that convert float to an integer with other rounding
    modes.</p>

    <p>Bool converted to 0 (false) or 1 (true).
    Yes, unlike most languages that usually
    don't guarantee "true" value (saying "true" is anything &lt;&gt; 0),
    KambiScript actually guarantees that "true" will result in 1.
    This is sometimes useful is smart mathematical expressions
    (<tt>my_int := 0.5 - (1 - my_bool(value))/2</tt>.</p>

    <p>String is converted to int by, well,
    converting string to integer using standard integer notation
    (<tt>int('123') = 123</tt>).</p></li>

  <li><p><a name="#function_float"><tt>float(...)</tt></a> converts a "core" type
    to a float.</p>

    <p>Integer is converted obviously. Actually it's never needed to
    explicitly cast integer to float, this conversion happens automatically,
    like in most programming languages.</p>

    <p>Bool is converted to
    obviously. Actually it's never needed to
    explicitly cast integer to float, this conversion happens automatically,
    like in most programming languages.</p>

    <p>String is converted to float by parsing number from string,
    like <tt>float('3.14') = 3.14</tt>.</p></li>

  <li><p><a name="#function_bool"><tt>bool(...)</tt></a> converts a "core" type
    to a boolean.</p>

    <p>Integers and floats are converted to "false" if equal zero, "true"
    otherwise.</p>

    <p>Strings cannot be converted to booleans, as I couldn't imagine
    any definition that would be universally useful here.</p></li>

  <li><p><a name="#function_string"><tt>string(...)</tt></a> converts a "core" type
    to a string.</p>

    <p>Not much to write here, numbers (integers and floats) are converted
    to normal notation and boolean is converted to 'false' or 'true'.</p></li>
</ul>

<p>All four basic conversion functions accept also variables that already
have the necessary type. For example, converting float to float is a valid
(and harmless) operation.</p>

<p>Compound types operations:</p>

<ul>
  <li><p><i>Arrays</i>:</p>

    <p><a name="#function_array"><tt>array(item1, item2, ...)</tt></a>
    constructs an array. At least one argument is required.
    All arguments must have the same type (VRML multiple-value fields
    can't have mixed types).</p>

    <p>Note that parameter-less <tt>array()</tt> call is not allowed,
    because we wouldn't know then the resulting type (is it an
    empty array of floats? empty array of integers? etc.)
    Don't worry, you can use <tt>array_set_count(my_array, 0)</tt> for this.</p>

    <p>Note that floating-point values in arrays are stored only with single-
    or double- precision. This contrasts with singleton values, which are always stored
    in the best precision possible. Having explicit single-
    or double- precision arrays is better for storage and allows faster
    copying between VRML fields. Normal <tt>array</tt> with float parameters will create
    an array of single-precision values (that is, VRML <tt>MFFloat</tt>).
    You have to call <tt>array_d</tt> to request double-precision storage
    (suitable for VRML <tt>MFDouble</tt> or <tt>MFTime</tt>).</p>

    <p><tt>array_count(my_array)</tt> and
    <tt>array_set_count(my_array, new_count)</tt> get and set array count.
    When you grow array, newly added items have undefined values.
    When you shrink array, excessive values are discarded.</p>

    <p><a name="#array_get"><tt>array_get(my_array, index)</tt></a>
    gets an item from array on given index. In "normal" programming languages,
    implemented by less lazy programmers, this is written as <tt>my_array[index]</tt> :)
    Analogous
    <a name="#array_set"><tt>array_set(my_array, index, component_value)</tt></a>
    sets a value of item in an array.
    In "normal" programming languages you would write <tt>my_array[index] := component_value</tt>.

  <li><p><i>Vectors</i>:</p>

    <p><a name="#function_vector"><tt>vector(x, y)</tt>, <tt>vector(x, y, z)</tt>, <tt>vector(x, y, z, w)</tt></a>
    create a single-precision vectors (called <tt>SFVec2f</tt>,
    <tt>SFVec3f</tt>, <tt>SFVec4f</tt> in VRML).
    Suffix <tt>_d</tt> means that you want double-precision vectors:
    <tt>vector_d(x, y)</tt>, <tt>vector_d(x, y, z)</tt>, <tt>vector_d(x, y, z, w)</tt>.</p>

    <p><a name="#vector_get"><tt>vector_get(my_vec, index)</tt></a>
    gets vector component. Allowed index values obviously depend on vector size,
    for example on <tt>SFVec3f</tt> you can use index 0, 1, 2.
    <a name="#vector_set"><tt>vector_set(my_vec, index, component_value)</tt></a>
    sets given vector component.</p>

    <p><tt>vector_get_count</tt> is available, for analogy with
    <tt>array_get_count</tt>.</p>

    <p>Standard vector math utilities are available:
    <tt>vector_length(v)</tt>, <tt>vector_sqr_length(v)</tt>,
    <tt>vector_dot(v1, v2)</tt>  (see <a href="http://en.wikipedia.org/wiki/Dot_product">vector dot product in wikipedia</a>),
    <tt>vector_cross(v1, v2)</tt> (see <a href="http://en.wikipedia.org/wiki/Cross_product">vector cross product in wikipedia</a>).
    </li>

  <li>TODO: string.
  <li>TODO: matrix.
  <li>TODO: image.
  <li>TODO: node.
</ul>

<p>Boolean operators:
<tt>Or</tt>, <tt>And</tt>, <tt>Not</tt> &mdash; self-explanatory
operations on booleans.</p>

<p>Mathematical functions (work on float type):</p>

<ul>
  <li><tt>Sin</tt>, <tt>Cos</tt>, <tt>Tan</tt>, <tt>CoTan</tt>
  <li><tt>ArcSin</tt>, <tt>ArcCos</tt>, <tt>ArcTan</tt>, <tt>ArcCoTan</tt>
  <li><tt>SinH</tt>, <tt>CosH</tt>, <tt>TanH</tt>, <tt>CoTanH</tt>
  <li><tt>Log2</tt>, <tt>Ln</tt>, <tt>Log</tt>, <tt>Power2</tt>,
    <tt>Exp</tt>, <tt>Power</tt>, <tt>Sqr</tt>, <tt>Sqrt</tt><br>
    (<tt>Log2(x) = Log(2, x)</tt>,
     <tt>Power2(x) = Power(2, x) = 2^x</tt>,
     <tt>Exp(x) = Power(enat, x) = enat^x</tt>)
  <li><tt>Sgn</tt> (returns integer), <tt>Abs</tt>
</ul>

<?php echo $toc->html_section(); ?>

<pre class="light_bg">
  Operand (aka "something that can be assigned") = Identifier

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

  Constant = "pi" | "enat" |
             Digit [{Digit}] ["." Digit [{Digit}] ] |
             "true" | "false" |
             string constant in apostrophes

  FunctionName = (see list of built-in functions above)
</pre>

<p>Generally, at least one whitespace is required between two tokens.
But there are many exceptions, when situation is unambiguous,
for example no whitespace is needed between
identifiers and parenthesis.
In case of uncertainty lexer is greedy, that is lexer tries to eat as much
characters as it can for a single token.</p>

<p>Case sensitivity: language is not case-sensitive.
That said, in the future in may be partially case-sensitive,
in places where you specify field/event names of VRML
since <i>whole VRML is case-sensitive</i>. So always specify
VRML field/event names with matching case.

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
