#[test]
fn parse_annotation() {
    test_parse("@icon(\"res://path/to/optional/icon.svg\")");
}

#[test]
fn parse_class_name() {
    test_parse("class_name MyClass");
}

#[test]
fn parse_extends() {
    test_parse("extends BaseClass");
}

#[test]
fn parse_literal_int() {
    test_parse("var a = 5");
}

#[test]
fn parse_literal_string() {
    test_parse("var a = \"Hello\"");
}

#[test]
fn parse_literal_array() {
    test_parse("var a = [1, 2, 3]");
}

#[test]
fn parse_literal_dict() {
    test_parse("var a = {\"key\": \"value\", 2: 3}");
}

#[test]
fn parse_literal_dict_2() {
    test_parse("var a = {key = \"value\", other_key = 2}");
}

#[test]
fn parse_typed_var() {
    test_parse("var typed_var: int");
}

#[test]
fn parse_inferred_type_var() {
    test_parse("var inferred_type := \"String\"");
}

#[test]
fn parse_enums() {
    test_parse(
        "
enum {UNIT_NEUTRAL, UNIT_ENEMY, UNIT_ALLY}
enum Named {THING_1, THING_2, ANOTHER_THING = -1}",
    );
}

#[test]
fn parse_vector() {
    test_parse("var v2 = Vector2(1, 2)");
}

#[test]
fn parse_function_1() {
    test_parse(
        "
func some_function(param1, param2, param3):
    const local_const = 5

    if param1 < local_const:
        print(param1)
    elif param2 > 5:
        print(param2)
    else:
        print(\"Fail!\")

    for i in range(20):
        print(i)

    while param2 != 0:
        param2 -= 1

    match param3:
        3:
            print(\"param3 is 3!\")
        _:
            print(\"param3 is not 3!\")

    var local_var = param1 + 3
    return local_var
",
    );
}

#[test]
fn parse_function_2() {
    test_parse(
        "
func something(p1, p2):
    super(p1, p2)
",
    );
}

#[test]
fn parse_function_3() {
    test_parse(
        "
func other_something(p1, p2):
    super.something(p1, p2)
",
    );
}

#[test]
fn parse_inner_class() {
    test_parse(
        "
class Something:
    var a = 10
",
    );
}

#[test]
fn parse_constructor() {
    test_parse(
        "
func _init():
    print(\"Constructed!\")
    var lv = Something.new()
    print(lv.a)
",
    );
}

#[cfg(test)]
fn test_parse(code: &str) {
    use super::parse::parse;
    use crate::utils::slice::Slice;
    use std::rc::Rc;

    let i = Slice::new(Rc::new(String::from(code)));
    let parsed = parse(i);

    assert!(
        parsed.is_ok(),
        "\nFailed to parse:\n\n{}\n\n{:?}\n\n",
        code,
        parsed
    );
}
