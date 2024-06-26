---
glyph: '○'
pattern: 'r←x○y'
name: Circular
---

`r` is the result of the scalar application of the circular function. The left argument selects an operation to be applied on the right argument:

|`x`|`x○y`|
|---|-----|
|`0`|$\sqrt{1 - y^2}$|
|`1`|$\sin y$|
|`¯1`|$\arcsin y$|
|`2`|$\cos y$|
|`¯2`|$\arccos y$|
|`3`|$\tan y$|
|`¯3`|$\arctan y$|
|`4`|$\sqrt{1 + y^2}$|
|`¯4`|$\sqrt{y^2 - 1}$|
|`5`|$\sinh y$|
|`¯5`|$\mathop{\text{arsinh}} y$|
|`6`|$\cosh y$|
|`¯6`|$\mathop{\text{arcosh}} y$|
|`7`|$\tanh y$|
|`¯7`|$\mathop{\text{artanh}} y$|
|`8`|$\sqrt{-1 - y^2}$|
|`¯8`|$-\sqrt{-1 - y^2}$|
|`9`|$\mathop{\text{Re}} y$|
|`¯9`|$y$|
|`10`|$\mid y \mid$|
|`¯10`|$\overline y$ (conjugate of $y$)|
|`11`|$\mathop{\text{Im}} y$|
|`¯11`|$iy$|
|`12`|$\mathop{\text{Arg}} y$|
|`¯12`|$e^{iy}$|

Any other value for `x` is an error.