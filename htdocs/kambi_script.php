<?php
  require 'vrmlengine_functions.php';
  common_header('KambiScript language', LANG_EN);

  function func_ref($name, $title)
  {
    echo '<a href="#function_' . $name . '"><tt>' . $title . '</tt></a>';
  }

  function func($name, $title)
  {
    echo '<a name="function_' . $name . '" class="kscript_func_docs"><tt>' . $title . '</tt></a>';
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
<?php func_ref('if', 'if(condition, then_code, else_code)'); ?>.
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
      new TocItem('Type conversion', 'functions_conversion', 1),
      new TocItem('Control flow', 'functions_control_flow', 1),
      new TocItem('Number (integer and float) functions', 'functions_number', 1),
      new TocItem('Boolean functions', 'functions_boolean', 1),
      new TocItem('String functions', 'functions_string', 1),
      new TocItem('Array functions', 'functions_array', 1),
      new TocItem('Vector functions', 'functions_vector', 1),
      new TocItem('Matrix functions', 'functions_matrix', 1),
      new TocItem('Image functions', 'functions_image', 1),
      new TocItem('VRML node functions', 'functions_node', 1),
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
# Simple converter from SFString to MFString using built-in <?php func_ref('array', 'array'); ?> function.
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

<p><i>Comments</i> are within curly braces: <tt>{ this is a comment }</tt>
(Pascal-like).</p>

<?php echo $toc->html_section(); ?>

<p><i>Types</i> are never explicitly declared, and are checked
at runtime. Four core types are available:</p>

<ol>
  <li><p>Integers. (64-bit integer; although note that for VRML long/int32
    fields, this will have to fit within 32-bit anyway.)
    Syntax of integer constants is obvious,
    like <tt>123</tt>. Built-in function
    <?php func_ref('int', 'int(...)'); ?> allows
    you to convert other core types into integer.</p></li>

  <li><p>Floats. (Uses the best floating-point type precision on given
    platform, which means at least Double, but on many platforms Extended.)
    Syntax of float constants
    is also obvious, like <tt>3.1415</tt>. You have also
    constants <tt>pi</tt> and <tt>enat</tt> (Euler's number).
    Built-in function
    <?php func_ref('float', 'float(...)'); ?> allows
    you to convert other core types into float.</p></li>

  <li><p>Booleans. Two obvious constants are available, <tt>false</tt>
    and <tt>true</tt> (case is ignored, as usual in KambiScript,
    so you can also write uppercase
    <tt>FALSE</tt> or <tt>TRUE</tt> like in classic VRML).
    Built-in function
    <?php func_ref('bool', 'bool(...)'); ?> allows
    you to convert other core types into boolean.</p></li>

  <li><p>Strings. Syntax of constants is Pascalish (in apostrophes, and two
    consecutive apostrophes inside mean that you want a single literal
    apostrophe character). For example <tt>'He said "It''s mine."'</tt>.
    Apostrophe was chosen not only because, y'know, it's Pascalish :),
    but also because it makes embedding KambiScript code within
    VRML string easier (no need to escape quotes by backslashes).
    You can make actual newlines within the string, like in VRML.
    For example:
<pre class="light_bg">
Script {
  # Let's assume some TouchSensor.touchTime is routed here.
  inputOnly SFTime touch_time
  outputOnly MFString text

  url "kambiscript:
function touch_time(value, timestamp)
  text := array(
    'First string of text clicked on ' + string(value),
    'Second string of text.
Still second string of text.
As you see, you can simply make a newline in the string literal to get a newline inside the string.'
  )
"
}
</pre>

    <p>Built-in function
    <?php func_ref('string', 'string(...)'); ?> allows
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
<?php func_ref('vector', 'vector(x, y, z)'); ?> ,
reader for particular component <?php func_ref('vector_get', 'vector_get(vector, index)'); ?>,
and setter for particular component <?php func_ref('vector_set', 'vector_set(vector, index, component_value)'); ?>.
Even images have functions to create and modify them, which means
that you can use KambiScript to perform basic image creation and processing.</p>

<p>Also array types are internally available, for VRML multiple-value
(MFXxx) types. Again no special syntax is available (sorry, no bracket parenthesis),
but there are functions to construct array
<?php func_ref('array', 'array(item1, item2, ...)'); ?>,
read component <?php func_ref('array_get', 'array_get(array, index)'); ?> and
set component <?php func_ref('array_set', 'array_set(array, index, component_value)'); ?>.

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

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php func('int', 'int(...)'); ?> converts a "core" type
    to an integer.</p>

    <p>Float is converted to int by discarding it's fractional
    part (like in C; for positive numbers, this is like <tt>floor</tt>, for negative
    this is like <tt>ceil</tt>).
    There are also functions <?php func('floor', 'floor'); ?>, <?php func('ceil', 'ceil'); ?> and
    <?php func('round', 'round'); ?> that convert float to an integer with other rounding
    modes.</p>

    <p>Bool is converted to 0 (false) or 1 (true).
    Yes, unlike most languages that usually
    don't guarantee "true" value (saying "true" is anything &lt;&gt; 0),
    KambiScript actually guarantees that "true" will result in 1.
    This is sometimes useful is smart mathematical expressions
    (<tt>my_int := 0.5 - (1 - my_bool(value))/2</tt>.</p>

    <p>String is converted to int by, well,
    converting string to integer using standard integer notation
    (<tt>int('123') = 123</tt>).</p></li>

  <li><p><?php func('float', 'float(...)'); ?> converts a "core" type
    to a float.</p>

    <p>Integer is converted obviously. Actually it's never needed to
    explicitly cast integer to float, this conversion happens automatically,
    like in most programming languages.</p>

    <p>Bool is converted to 0.0 (false) or 1.0 (true).</p>

    <p>String is converted to float by parsing number from string,
    like <tt>float('3.14') = 3.14</tt>.</p></li>

  <li><p><?php func('bool', 'bool(...)'); ?> converts a "core" type
    to a boolean.</p>

    <p>Integers and floats are converted to "false" if equal zero, "true"
    otherwise.</p>

    <p>Strings are converted to booleans recognizing 'false' and 'true'
    strings (and making errors in other cases).</p></li>

  <li><p><?php func('string', 'string(...)'); ?> converts a "core" type
    to a string.</p>

    <p>Not much to write here, numbers (integers and floats) are converted
    to normal notation and boolean is converted to 'false' or 'true'.</p></li>
</ul>

<p>All four basic conversion functions accept also variables that already
have the necessary type. For example, converting float to float is a valid
(and harmless) operation.</p>

<?php echo $toc->html_section(); ?>

<p><?php func('if', 'if(condition, then_code, else_code)'); ?> is our
conditional instruction. <tt>condition</tt> is first calculated, must be a boolean
value. If it's true, then <tt>then_code</tt> is executed and returned as
"if" value. Otherwise, <tt>else_code</tt> is executed and returned as
"if" value. You may note that (because of KambiScript unification of
"instruction" and "expression" terms) this can be used in both
functional and imperative form. That is, all below are valid:</p>

<pre class="light_bg">
  { imperative form }
  if(x &gt; 3, y := 'yes', y := 'no');

  { functional form, equivalent to above, looks little more elegant in this case }
  y := if(x &gt; 3, 'yes', 'no');

  { actually, even this is possible if you need it: }
  y_copy := if(x &gt; 3, y := 'yes', y:= 'no');
</pre>

<p><?php func('while', 'while(condition, loop_code)'); ?> performs
a while loop. Calculate <tt>condition</tt> (must yield a boolean value),
if true then execute <tt>loop_code</tt> and again calculate <tt>condition</tt>,
if it's still true then execute <tt>loop_code</tt> again, ... you get the idea.</p>

<p><?php func('for', 'for(counter, begin_value, end_value, loop_code)'); ?> performs
a for loop. <tt>counter</tt> must be an assignable integer variable
(note that for now you cannot declare new variables for KambiScript;
you usually need to overuse <tt>initializeOnly</tt> field or VRML script
node for this). <tt>begin_value</tt>, <tt>end_value</tt> must also
be integer values, will be calculated at the beginning.
We will to assign <tt>counter</tt> variable integer values
from <tt>begin_value</tt> to <tt>end_value</tt>, and for each
occurrence counter value we will execute <tt>loop_code</tt>.
It's undefined what happens when <tt>loop_code</tt> changes directly the
<tt>counter</tt>value.</p>

<p><tt>for</tt> and <tt>while</tt> loops return
the value of last executed <tt>loop_code</tt>,
or <tt>false</tt> if <tt>loop_code</tt> did not get executed even once.</p>

<?php echo $toc->html_section(); ?>

<p>Self-explanatory math functions are listed below.
They all take a float type, and return a float type unless otherwise noted:</p>

<ul>
  <li><?php func('Sin', 'Sin'); ?>,
      <?php func('Cos', 'Cos'); ?>,
      <?php func('Tan', 'Tan'); ?>,
      <?php func('CoTan', 'CoTan'); ?>
  <li><?php func('ArcSin', 'ArcSin'); ?>,
      <?php func('ArcCos', 'ArcCos'); ?>,
      <?php func('ArcTan', 'ArcTan'); ?>,
      <?php func('ArcCoTan', 'ArcCoTan'); ?>
  <li><?php func('SinH', 'SinH'); ?>,
      <?php func('CosH', 'CosH'); ?>,
      <?php func('TanH', 'TanH'); ?>,
      <?php func('CoTanH', 'CoTanH'); ?>
  <li><?php func('Log2', 'Log2'); ?> (same as <tt>Log(2, x)</tt>),
      <?php func('Ln', 'Ln'); ?>,
      <?php func('Log', 'Log'); ?>,
      <?php func('Power2', 'Power2'); ?> (same as <tt>Power(2, x) = 2^x</tt>),
      <?php func('Exp', 'Exp'); ?> (same as <tt>Power(enat, x) = enat^x</tt>),
      <?php func('Power', 'Power'); ?>,
      <?php func('Sqr', 'Sqr'); ?>,
      <?php func('Sqrt', 'Sqrt'); ?><br>
  <li><?php func('Sgn', 'Sgn'); ?> (returns integer), <?php func('Abs', 'Abs'); ?>
</ul>

<?php echo $toc->html_section(); ?>

<p>Basic boolean operations:
<?php func('or(a, b)', 'or(a, b)'); ?>, <?php func('and(a, b)', 'and(a, b)'); ?>, <?php func('not(a)', 'not(a)'); ?>.</p>

<?php echo $toc->html_section(); ?>

TODO

<?php echo $toc->html_section(); ?>

<p><?php func('array', 'array(item1, item2, ...)'); ?>
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
You have to call <?php func('array_d', 'array_d'); ?> to request double-precision storage
(suitable for VRML <tt>MFDouble</tt> or <tt>MFTime</tt>).</p>

<p><?php func('array_count(my_array)', 'array_count(my_array)'); ?> and
<?php func('array_set_count(my_array, new_count)', 'array_set_count(my_array, new_count)'); ?> get and set array count.
When you grow array, newly added items have undefined values.
When you shrink array, excessive values are discarded.</p>

<p><?php echo func('array_get', 'array_get(my_array, index)'); ?>
 gets an item from array on given index. In "normal" programming languages,
implemented by less lazy programmers, this is written as <tt>my_array[index]</tt> :)
Analogous
<?php echo func('array_set', 'array_set(my_array, index, component_value)'); ?>
 sets a value of item in an array.
In "normal" programming languages you would write <tt>my_array[index] := component_value</tt>.

<?php echo $toc->html_section(); ?>

<p><?php func('vector', 'vector(x, y), vector(x, y, z), vector(x, y, z, w)'); ?>
 create a single-precision vectors (called <tt>SFVec2f</tt>,
<tt>SFVec3f</tt>, <tt>SFVec4f</tt> in VRML).
Suffix <tt>_d</tt> means that you want double-precision vectors:
<?php func('vector_d', 'vector_d(x, y), vector_d(x, y, z), vector_d(x, y, z, w)'); ?>.</p>

<p><?php echo func('vector_get', 'vector_get(my_vec, index)'); ?>
 gets vector component. Allowed index values obviously depend on vector size,
for example on <tt>SFVec3f</tt> you can use index 0, 1, 2.
<?php echo func('vector_set', 'vector_set(my_vec, index, component_value)'); ?>
 sets given vector component (and returns new vector, for comfort).</p>

<p><?php func('vector_get_count', 'vector_get_count(my_vec)'); ?> is available,
for analogy with <tt>array_get_count</tt>. Vector has a fixed number
of components, so there is no <tt>vector_set_count</tt>.
</p>

<p>Standard vector math utilities are available:
<?php func('vector_length(v)', 'vector_length(v)'); ?>, <?php func('vector_sqr_length(v)', 'vector_sqr_length(v)'); ?>,
<?php func('vector_dot(v1, v2)', 'vector_dot(v1, v2)'); ?>  (see <a href="http://en.wikipedia.org/wiki/Dot_product">vector dot product in wikipedia</a>),
<?php func('vector_cross(v1, v2)', 'vector_cross(v1, v2)'); ?> (see <a href="http://en.wikipedia.org/wiki/Cross_product">vector cross product in wikipedia</a>,
only on 3d vectors).

<?php echo $toc->html_section(); ?>

TODO

<?php echo $toc->html_section(); ?>

TODO

<?php echo $toc->html_section(); ?>

TODO

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
  # "/" does division. Like in C, when both operands are integers,
  #     this performs an integer division (that is, it's the floor of
  #     actual division result, corresponding to Pascal's "div" operator).
  #     When either operand is float then this is normal float division
  #     (more precisely, if only one operand is float, the other will be
  #     promoted to float then; and then float division will be done.)

  Term = Factor [{FactorOperator Factor}]
  TermOperator = "+" | "-"

  ComparisonArgument = Term [{TermOperator Term}]
  ComparisonOperator = "&lt;" | "&gt;" | "&lt;=" | "&gt;=" | "=" | "&lt;&gt;"

  # Note that comparisons on float types (this also includes vectors, matrices
  # and arrays based on float types) perform <i>exact</i> comparison
  # (like in all programming languages).
  # This means that adding 1.0 one hundred times will not necessarily yield result
  # equal to literal 100.0. You can compare with some epsilon, like
  # "abs(a-b) < 0.001", if needed.

  NonAssignmentExpression = ComparisonArgument [{ComparisonOperator ComparisonArgument}] |

  # Programmers using our engine: note that KambiScriptParser.ParseFloatExpression
  # parses exactly "NonAssignmentExpression" token, as defined above,
  # with the Factor definition hacked to also allow only NonAssignmentExpression
  # inside parenthesis. In other words, ParseFloatExpression takes care to only
  # parse a calculated expression, without any assignments or sequence.

  PossiblyAssignmentExpression = NonAssignmentExpression |
                                 Operand ":=" PossiblyAssignmentExpression

  Expression = PossiblyAssignmentExpression [{";" PossiblyAssignmentExpression}]

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
