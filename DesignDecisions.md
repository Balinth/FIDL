# Design decisions and the reasoning behind them

## Syntax decisions:

### Records, choice types:

Here the most important thing is to make it painfully obvious at a glance, that a type is a choice type, or a record type.
To this end:
1. record and choice types should be prefixed with "record" or "choice" respectively, to make it explicit even in simple cases (only 1 type field, single line types etc).
2. record and choice types should use different tokens: "{", ";", "}" for record, and "=", "|" for choice types for complex cases (type with many fields)
3. only one of them can have an optional field delimiter (out of ";" and "|"), to make sure they are still visually well separable with only mandatory characters
4. the optional delimiter is the record's ";", because it is expected to be the more often used type.
