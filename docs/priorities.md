# Priorities

(In order of importance.)

### 1. Usefulness as a way of describing data

Plate schemas should be expressive enough to describe all the data that passes through many applications. At the same they should be able to provide meaningful structure to that data.

### 2. Unchanging in a principled way

This is different from simple backwards compatibility. The goal is to eventually reach a place where each decision made by Plate follows from principled reasons. If we end up doing something like adding new validators because they seem useful at the moment then something is wrong.

Inspired by [the ECMA 404 introduction](https://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf): "Because it is so simple, it is not expected that the JSON grammar will ever change."

### 3. Efficient data serialization

This is a low priority for a reason. Tools like [Cap’n Proto](https://capnproto.org/) already exist to move data around in an extraordinarily efficient way. Plate exists so if you want to make a new meaningful-on-its-own format like [iCalendar](https://en.wikipedia.org/wiki/ICalendar) you can describe it well.

These two approaches complement each other. For example you might use [Cap’n Proto](https://capnproto.org/) representations of your data in your APIs, but provide Plate representations on export.

# Not priorities

### + Efficient schema serialization

With regards to performance schemas get passed around a lot less than normal data. So it doesn't matter much how big they are.

With regards to human-readability we'll eventually write a DSL for them (which may look a lot like Haskell source code). So the readability of JSON-encoded schemas won't matter much in the long run.

### + Ability to describe existing data types with Plate

Not all (eg) JSON values can be converted to Plate values.

Of those that can, not all Plate values can be described by Plate schemas. For instance, you can express untagged unions in Plate values, but not in Plate schemas.

This is OK. There are an unlimited number of ways to contort values, a good type system can't cover all of them. Plate is meant to be used during the creation of new data types. It's not meant to describe existing ones retroactively.

### + Comprehensive UI descriptions

Plate is a schema language, not a DSL for UIs. We want it to be useful for generating UIs, but it would be impossible for Plate to describe everything that goes into a UI: font choices, background color, horizontal vs. vertical positioning, etc.

At some point a line has to be drawn. I think the cleanest place to do this is to say that Plate will only include information that's relevent to validating data. This is enough to auto-generate simple UIs.

In the future when our UI-generation tools become more sophisticated we can consider various "UI-specification-formats" to be used alongside of Plate schemas. They can describe nuances of UIs that can't be expressed in the schemas themselves.
