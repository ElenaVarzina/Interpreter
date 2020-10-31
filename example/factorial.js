n = toNumber(input());
i = 1;
r = 1;
while (i <= n) {
  r = r * i;
  i = i + 1;
}
print(n, "factorial", "equals", r);
