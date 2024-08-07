---
name: Trains
planned: true
---

Unlike in many other APLs, trains in TinyAPL are written using special syntax: `⦅`, `⋄` and `⦆`. They are a sequence of arrays, functions, adverbs and conjunctions that combine to create functions or modifiers.

## 1-trains

The simplest train is the 1-train:

* `⦅x⦆` is `x⍨`;
* `⦅F⦆` is `F`;
* `u _⦅_A⦆` is `u _A`;
* `u _⦅_C_⦆_ v` is `u _C_ v`;

## 2-trains

* `⦅x⋄y⦆` is `x⍨`
* `⦅x⋄G⦆` is `x∘G`
* `⦅F⋄y⦆` is `F∘y`
* `⦅F⋄G⦆` is `F⍤G`
* `⦅x⋄_A⦆` is `x _A`
* `⦅F⋄_A⦆` is `F _A`
* `u _⦅_A⋄G⦆` is `(u _A)⍤G`
* `u _⦅_A⋄_B⦆` is `(u _A) _B`
* `u _⦅_A⋄_C_⦆` is `(u _A) _C_ u` *should this be changed to Over-like `(u _A) _C_ (v _A)`?
* `u _⦅x⋄_C_⦆` is `x _C_ u`
* `u _⦅F⋄_C_⦆` is `F _C_ u`
* `u _⦅_C_⋄y⦆` is `u _C_ y`
* `u _⦅_C_⋄G⦆` is `u _C_ G`
* `u _⦅_C_⋄_A⦆_ v` is `(u _C_ v) _A`
* `u _⦅_C_⋄_D_⦆_ v` is `(u _C_ v)⍤(u _D_ v)`

## 3-trains

* `⦅x⋄y⋄z⦆` is `y⍨`
* `⦅x⋄y⋄H⦆` is `y⍨`
* `⦅F⋄y⋄x⦆` is `y⍨`
* `⦅F⋄y⋄H⦆` is `y⍨`
* `⦅F⋄G⋄H⦆` is `F«G»H`
* `⦅x⋄G⋄H⦆` is `(x∘G)⍤H`
* `⦅F⋄G⋄z⦆` is `(G∘z)⍤F`
* `⦅x⋄G⋄z⦆` is `(x G z)⍨`
* `⦅x⋄_C_⋄z⦆` is `x _C_ z`
* `⦅x⋄_C_⋄H⦆` is `x _C_ H`
* `⦅F⋄_C_⋄z⦆` is `F _C_ z`
* `⦅F⋄_C_⋄H⦆` is `F _C_ H`
* `u _⦅_A⋄G⋄H⦆` is `(u _A)«G»H`
* `u _⦅_A⋄_B⋄_C⦆` is `((u _A) _B) _C`
* `u _⦅x⋄_C_⋄_A⦆` is `x _C_ (u _A)`
* `u _⦅F⋄_C_⋄_A⦆` is `F _C_ (u _A)`
* `u _⦅_A⋄_C_⋄z⦆` is `(u _A) _C_ z`
* `u _⦅_A⋄_C_⋄H⦆` is `(u _A) _C_ H`
* `u _⦅F⋄G⋄_C_⦆_ v` is `F«G»(u _C_ v)`
* `u _⦅x⋄G⋄_C_⦆_ v` is `(x∘G)⍤(u _C_ v)`
* `u _⦅_C_⋄G⋄H⦆_ v` is `(u _C_ v)«G»H`
* `u _⦅_C_⋄G⋄_D_⦆_ v` is `(u _C_ v)«G»(u _D_ v)`
* `u _⦅_A⋄_B⋄H⦆_ v` is `(u _A)«(v _B)»H`
* `u _⦅_C_⋄_A⋄_B⦆_ v` is `((u _C_ v) _A) _B`
* `u _⦅x⋄_C_⋄_D_⦆_ v` is `x _C_ (u _D_ v)`
* `u _⦅F⋄_C_⋄_D_⦆_ v` is `F _C_ (u _D_ v)`
* `u _⦅_A⋄_C_⋄_B⦆_ v` is `(u _A) _C_ (v _B)`
* `u _⦅_A⋄_C_⋄_D_⦆ v` is `(u _A) _C_ (u _D_ v)`
* `u _⦅_C_⋄_D_⋄z⦆_ v` is `(u _C_ v) _D_ z`
* `u _⦅_C_⋄_D_⋄H⦆_ v` is `(u _C_ v) _D_ H`
* `u _⦅_C_⋄_D_⋄_A⦆_ v` is `(u _C_ v) _D_ (v _A)`
* `u _⦅_C_⋄_D_⋄_E_⦆_ v` is `(u _C_ v) _D_ (u _E_ v)`

## Longer trains

Longer trains are parsed right-to-left, two tines at a time, creating 3-trains; the result of an evaluation becomes the rightmost tine of the next group. If only a tine is left it is used to create a 2-train. If the left tine of a 3-train is empty, it becomes a 2-train. This can be useful to force a chain of functions to be an Atop instead of a Fork.
