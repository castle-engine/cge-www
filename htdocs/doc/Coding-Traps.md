## Changing only a temporary record value

Using

```
Scene.Translation.Z := Scene.Translation.Z + 0.1;
```

is unfortunately a mistake, and compiler doesn't warn about it. As `Scene.Translation` is an advanced record (`TVector3`), using `Scene.Translation` may return just a temporary copy of it (just as if you would call a method `Scene.GetTranslation`). So then you change the `Z` parameter of this temporary copy, and the effect gets ignored.

The solution is to only modify the record fields (like `Z` of a `TVector3`) when you know that it is a simple field, or variable in your code. To apply this solution above, you should make a temporary variable and change it:

```
var
  V: TVector3;
begin
  V := Scene.Translation;
  V.Z := V.Z + 0.1;
  Scene.Translation := V;
end;
```

You can also rework it to assign whole vector, like this:

```
Scene.Translation := Scene.Translation + Vector3(0, 0, 0.1);
```

The problem is inherent to using properties with records. (If you're curious, C# has this problem too with its `struct`s, in Unity too.) But we cannot easily avoid doing it in CGE -- we need records (for things like vectors, records are both more efficient and more comfortable than classes) and we need properties of them (because setters often have to do some useful things).
