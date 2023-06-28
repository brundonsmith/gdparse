use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::one_of,
    combinator::{map, map_opt, opt},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{pair, preceded, terminated, tuple},
    Parser,
};

use crate::{
    make_node, make_node_tuple,
    utils::{
        parse_utils::{log, precedence, ParseError, ParseErrorDetails, ParseResult},
        slice::{Slicable, Slice},
    },
};

use super::ast::*;

macro_rules! seq {
    ($first:expr, $( $s:expr ),* $(,)?) => {
        tuple(( $first, $(w($s)),* ))
    };
}

pub fn parse(code: Slice) -> Result<AST<GDScript>, ParseError> {
    let res = terminated(
        many0(preceded(blank_lines_and_indentation(0), declaration(0))),
        blank_lines_and_indentation(0),
    )(code.clone());

    match res {
        Ok((i, mut declarations)) => {
            if i.len() > 0 {
                Err(ParseError {
                    src: i,
                    details: ParseErrorDetails::Incomplete,
                })
            } else {
                Ok(make_node!(
                    GDScript,
                    declarations[0].spanning(&declarations.last().unwrap()),
                    declarations
                ))
            }
        }
        Err(error) => Err(match error {
            nom::Err::Error(raw) => raw,
            nom::Err::Failure(raw) => raw,
            nom::Err::Incomplete(_) => unreachable!(),
        }),
    }
}

fn declaration(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<Declaration>> {
    move |i: Slice| -> ParseResult<AST<Declaration>> {
        alt((
            map(extends, AST::recast::<Declaration>),
            map(class_name, AST::recast::<Declaration>),
            map(val_declaration, AST::recast::<Declaration>),
            map(func_declaration(ind), AST::recast::<Declaration>),
            map(class_declaration(ind), AST::recast::<Declaration>),
            map(annotation, AST::recast::<Declaration>),
            map(enum_declaration, AST::recast::<Declaration>),
        ))(i)
    }
}

fn extends(i: Slice) -> ParseResult<AST<ExtendsDeclaration>> {
    map(
        seq!(break_after(tag("extends")), plain_identifier),
        |(start, mut extends_class)| {
            make_node!(
                ExtendsDeclaration,
                start.spanning(&extends_class),
                extends_class
            )
        },
    )(i)
}

fn class_name(i: Slice) -> ParseResult<AST<ClassNameDeclaration>> {
    map(
        seq!(break_after(tag("class_name")), plain_identifier,),
        |(start, mut class_name)| {
            make_node!(
                ClassNameDeclaration,
                start.spanning(&class_name),
                class_name
            )
        },
    )(i)
}

fn annotation(i: Slice) -> ParseResult<AST<Annotation>> {
    map(
        seq!(tag("@"), plain_identifier, args),
        |(start, mut name, mut arguments)| {
            make_node!(Annotation, start.spanning(&name), name, arguments)
        },
    )(i)
}

fn args(i: Slice) -> ParseResult<Vec<AST<Expression>>> {
    map(
        seq!(
            tag("("),
            separated_list0(w(tag(",")), w(expression(None))),
            tag(")")
        ),
        |(_, args, _)| args,
    )(i)
}

fn enum_declaration(i: Slice) -> ParseResult<AST<EnumDeclaration>> {
    map(
        seq!(
            break_after(tag("enum")),
            opt(plain_identifier),
            tag("{"),
            separated_list0(
                w(tag(",")),
                w(seq!(
                    plain_identifier,
                    opt(map(seq!(tag("="), int_literal), |(_, int)| {
                        int.recast::<Expression>()
                    }))
                ))
            ),
            tag("}")
        ),
        |(start, mut name, _, mut members, end)| {
            make_node!(EnumDeclaration, start.spanning(&end), name, members)
        },
    )(i)
}

fn func_declaration(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<FuncDeclaration>> {
    move |i: Slice| -> ParseResult<AST<FuncDeclaration>> {
        map(
            seq!(
                tag("func"),
                plain_identifier,
                tag("("),
                separated_list0(w(tag(",")), w(plain_identifier)),
                tag(")"),
                block(ind + 1)
            ),
            |(keyword, mut name, _, args, _, mut body)| {
                let mut is_static = false; // TODO
                let mut args: Vec<(AST<PlainIdentifier>, Option<AST<TypeExpression>>)> =
                    args.into_iter().map(|a| (a, None)).collect(); // TODO
                let mut return_type = None; // TODO

                make_node!(
                    FuncDeclaration,
                    keyword.spanning(&body),
                    is_static,
                    name,
                    args,
                    return_type,
                    body
                )
            },
        )(i)
    }
}

fn class_declaration(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<ClassDeclaration>> {
    move |i: Slice| -> ParseResult<AST<ClassDeclaration>> {
        map(
            pair(
                seq!(break_after(tag("class")), plain_identifier, tag(":")),
                many1(preceded(
                    blank_lines_and_indentation(ind + 1),
                    declaration(ind + 1),
                )),
            ),
            |((keyword, mut name, _), mut declarations)| {
                make_node!(
                    ClassDeclaration,
                    keyword.spanning(&declarations.last().unwrap()),
                    name,
                    declarations
                )
            },
        )(i)
    }
}

fn block(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<Block>> {
    move |i: Slice| -> ParseResult<AST<Block>> {
        map(
            pair(
                tag(":"),
                many1(preceded(blank_lines_and_indentation(ind), statement(ind))),
            ),
            |(_, mut statements)| {
                make_node!(
                    Block,
                    statements[0]
                        .slice()
                        .clone()
                        .spanning(statements.iter().last().unwrap()),
                    statements
                )
            },
        )(i)
    }
}

fn statement(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<Statement>> {
    move |i: Slice| -> ParseResult<AST<Statement>> {
        alt((
            map(tag("pass"), |src| Pass.as_ast(src).recast::<Statement>()),
            map(tag("break"), |src| Break.as_ast(src).recast::<Statement>()),
            map(val_declaration, AST::recast::<Statement>),
            map(
                map_opt(
                    function_call_property_chain,
                    AST::try_recast::<FunctionCall>,
                ),
                AST::recast::<Statement>,
            ),
            map(if_else_statement(ind), AST::recast::<Statement>),
            map(while_loop(ind), AST::recast::<Statement>),
            map(for_loop(ind), AST::recast::<Statement>),
            map(match_statement(ind), AST::recast::<Statement>),
        ))(i)
    }
}

fn val_declaration(i: Slice) -> ParseResult<AST<ValueDeclaration>> {
    map(
        seq!(
            alt((tag("var"), tag("const"))),
            plain_identifier,
            opt(type_declaration),
            pair(opt(tag(":")), opt(initial_value),)
        ),
        |(keyword, mut name, mut declared_type, (infer_symbol, mut value))| {
            let mut is_const = keyword.as_str() == "const";
            let mut is_inferred = infer_symbol.is_some();

            make_node!(
                ValueDeclaration,
                keyword.spanning(
                    value.as_ref().map(|v| v.slice()).unwrap_or(
                        declared_type
                            .as_ref()
                            .map(|d| d.slice())
                            .unwrap_or(name.slice())
                    )
                ),
                is_const,
                name,
                declared_type,
                is_inferred,
                value
            )
        },
    )(i)
}

fn if_else_statement(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<IfElseStatement>> {
    move |i: Slice| {
        map(
            tuple((
                seq!(break_after(tag("if")), expression(None), block(ind + 1)),
                many0(seq!(
                    break_after(tag("elif")),
                    expression(None),
                    block(ind + 1)
                )),
                opt(map(
                    seq!(break_after(tag("else")), block(ind + 1)),
                    |(_, block)| block,
                )),
            )),
            |((if_keyword, if_condition, if_outcome), elif_cases, else_case)| {
                let src = if_keyword.spanning(&if_outcome); // TODO
                let mut conditions = std::iter::once((if_condition, if_outcome))
                    .chain(
                        elif_cases
                            .into_iter()
                            .map(|(_, condition, outcome)| (condition, outcome)),
                    )
                    .collect::<Vec<(AST<Expression>, AST<Block>)>>();
                let mut default_outcome = else_case;

                make_node!(IfElseStatement, src, conditions, default_outcome)
            },
        )(i)
    }
}

fn while_loop(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<WhileLoop>> {
    move |i: Slice| {
        map(
            seq!(break_after(tag("while")), expression(None), block(ind + 1)),
            |(keyword, mut condition, mut body)| {
                make_node!(WhileLoop, keyword.spanning(&body), condition, body)
            },
        )(i)
    }
}

fn for_loop(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<ForLoop>> {
    move |i: Slice| {
        map(
            seq!(
                break_after(tag("for")),
                plain_identifier,
                tag("in"),
                expression(None),
                block(ind + 1)
            ),
            |(keyword, mut item_name, _, mut iteree, mut body)| {
                make_node!(ForLoop, keyword.spanning(&body), item_name, iteree, body)
            },
        )(i)
    }
}

fn match_statement(ind: usize) -> impl Fn(Slice) -> ParseResult<AST<MatchStatement>> {
    move |i: Slice| {
        map(
            pair(
                seq!(break_after(tag("match")), expression(None), tag(":"),),
                many1(preceded(
                    blank_lines_and_indentation(ind + 1),
                    match_case(ind + 1),
                )),
            ),
            |((keyword, mut subject, _), mut cases)| {
                make_node!(
                    MatchStatement,
                    keyword.spanning(&subject), // TODO
                    subject,
                    cases
                )
            },
        )(i)
    }
}

fn return_statement(i: Slice) -> ParseResult<AST<Return>> {
    map(
        seq!(break_after(tag("return")), opt(expression(None))),
        |(keyword, mut expr)| make_node!(Return, keyword, expr),
    )(i)
}

fn match_case(ind: usize) -> impl Fn(Slice) -> ParseResult<MatchCase> {
    move |i: Slice| {
        map(seq!(match_pattern, block(ind + 1)), |(pattern, outcome)| {
            MatchCase { pattern, outcome }
        })(i)
    }
}

fn match_pattern(i: Slice) -> ParseResult<AST<MatchPattern>> {
    map(
        separated_list1(w(tag(",")), w(expression_or_binding)),
        |mut members| {
            if members.len() == 1 {
                let mut expr = members.remove(0);
                let src = expr.slice().clone();
                let this = MatchPattern::Single(expr.clone()).as_ast(src);

                expr.set_parent(&this);

                this
            } else {
                let src = members
                    .first()
                    .unwrap()
                    .clone()
                    .spanning(members.last().unwrap());

                let this = MatchPattern::Multi(members.clone()).as_ast(src);

                members.set_parent(&this);

                this
            }
        },
    )(i)
    // TODO: array and dict patterns
}

fn expression_or_binding(i: Slice) -> ParseResult<ExpressionOrIdentifier> {
    alt((
        // TODO: Only allow constants and variables, not arbitrary expressions
        map(expression(None), ExpressionOrIdentifier::Expression),
        map(seq!(tag("var"), plain_identifier), |(_, ident)| {
            ExpressionOrIdentifier::Identifier(ident)
        }),
    ))(i)
}

fn type_declaration(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(seq!(tag(":"), typ), |(_, typ)| typ)(i)
}

fn initial_value(i: Slice) -> ParseResult<AST<Expression>> {
    map(seq!(tag("="), expression(None)), |(_, expression)| {
        expression
    })(i)
}

fn expression(
    after: Option<fn(Slice) -> ParseResult<AST<Expression>>>,
) -> impl Fn(Slice) -> ParseResult<AST<Expression>> {
    move |i: Slice| {
        precedence(
            &[
                &[function_call_property_chain],
                &[
                    dict_literal,
                    array_literal,
                    string_literal,
                    int_literal,
                    local_identifier,
                    boolean_literal,
                    null_literal,
                ],
            ],
            after,
            i,
        )
    }
}

fn function_call_property_chain(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            expression(Some(function_call_property_chain)),
            many1(w(alt((
                map(args, FunctionCallOrPropertyAccess::CalledWith),
                dot_property_access
            ))))
        ),
        |(base, clauses)| {
            let mut next_subject = base;

            for clause in clauses {
                let mut subject = next_subject;

                match clause {
                    FunctionCallOrPropertyAccess::CalledWith(mut args) => {
                        let mut function = subject;

                        next_subject = make_node!(
                            FunctionCall,
                            function.slice().clone(), // TODO
                            function,
                            args
                        )
                        .recast::<Expression>();
                    }
                    FunctionCallOrPropertyAccess::Accessing(mut property) => {
                        next_subject = make_node!(
                            PropertyAccess,
                            subject.spanning(&property),
                            subject,
                            property,
                        )
                        .recast::<Expression>();
                    }
                }
            }

            next_subject
        },
    )(i)
}

fn dot_property_access(i: Slice) -> ParseResult<FunctionCallOrPropertyAccess> {
    map(pair(tag("."), plain_identifier), |(_, ident)| {
        FunctionCallOrPropertyAccess::Accessing(ExpressionOrIdentifier::Identifier(ident))
    })(i)
}

#[derive(Debug, Clone)]
enum FunctionCallOrPropertyAccess {
    CalledWith(Vec<AST<Expression>>),
    Accessing(ExpressionOrIdentifier),
}

fn dict_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            tag("{"),
            separated_list0(
                w(tag(",")),
                w(map(
                    seq!(
                        alt((
                            string_literal,
                            int_literal,
                            map(plain_identifier, |ident| StringLiteral {
                                value: ident.downcast().name.clone(),
                                multiline: false
                            }
                            .as_ast(ident.downcast().name)
                            .recast::<Expression>())
                        )),
                        alt((tag(":"), tag("="))),
                        expression(None)
                    ),
                    |(key, _, value)| (key, value)
                ))
            ),
            tag("}"),
        ),
        |(open, mut entries, close)| {
            make_node!(DictionaryLiteral, open.spanning(&close), entries).recast::<Expression>()
        },
    )(i)
}

fn array_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            tag("["),
            separated_list0(w(tag(",")), w(expression(None))),
            tag("]"),
        ),
        |(open, mut members, close)| {
            make_node!(ArrayLiteral, open.spanning(&close), members).recast::<Expression>()
        },
    )(i)
}

fn string_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        alt((
            tuple((tag("\""), string_contents, tag("\""))),
            tuple((tag("'"), string_contents, tag("'"))),
            tuple((tag("\"\"\""), string_contents, tag("\"\"\""))),
        )),
        |(start, mut value, end)| {
            let mut multiline = start.as_str() == "\"\"\"";

            make_node!(StringLiteral, start.spanning(&end), value, multiline).recast::<Expression>()
        },
    )(i)
}

fn int_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        pair(opt(tag("-")), take_while1(|ch: char| ch.is_numeric())),
        |(neg, digits): (Option<Slice>, Slice)| {
            let mut value_raw = neg.map(|n| n.spanning(&digits)).unwrap_or(digits);
            make_node!(IntLiteral, value_raw.clone(), value_raw).recast::<Expression>()
        },
    )(i)
}

fn boolean_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(alt((tag("true"), tag("false"))), |x: Slice| {
        let mut value = x.as_str() == "true";
        make_node!(BooleanLiteral, x, value).recast::<Expression>()
    })(i)
}

fn null_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(tag("null"), |src| {
        NullLiteral.as_ast(src).recast::<Expression>()
    })(i)
}

fn local_identifier(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        take_while1(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$'),
        |name: Slice| {
            LocalIdentifier { name: name.clone() }
                .as_ast(name)
                .recast::<Expression>()
        },
    )(i)
}

fn typ(i: Slice) -> ParseResult<AST<TypeExpression>> {
    alt((
        map(tag("int"), |src| {
            IntType.as_ast(src).recast::<TypeExpression>()
        }),
        map(tag("float"), |src| {
            FloatType.as_ast(src).recast::<TypeExpression>()
        }),
        map(tag("String"), |src| {
            StringType.as_ast(src).recast::<TypeExpression>()
        }),
    ))(i)
}

fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    map(
        take_while1(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$'),
        |name: Slice| PlainIdentifier { name: name.clone() }.as_ast(name),
    )(i)
}

fn blank_lines_and_indentation(ind: usize) -> impl Fn(Slice) -> ParseResult<()> {
    move |i: Slice| -> ParseResult<()> {
        map(preceded(many0(consume_line), indentation(ind)), |_| ())(i)
    }
}

fn indentation(ind: usize) -> impl Fn(Slice) -> ParseResult<()> {
    move |i: Slice| -> ParseResult<()> {
        map(
            alt((
                tag(get_indentation_str_tabs(ind)),
                tag(get_indentation_str_spaces(ind)),
            )),
            |_: Slice| (),
        )(i)
    }
}

fn get_indentation_str_tabs(ind: usize) -> &'static str {
    match ind {
        0 => "",
        1 => "\t",
        2 => "\t\t",
        3 => "\t\t\t",
        4 => "\t\t\t\t",
        5 => "\t\t\t\t\t",
        _ => unimplemented!(),
    }
}

fn get_indentation_str_spaces(ind: usize) -> &'static str {
    match ind {
        0 => "",
        1 => "    ",
        2 => "        ",
        3 => "            ",
        4 => "                ",
        5 => "                    ",
        _ => unimplemented!(),
    }
}

fn string_contents(i: Slice) -> ParseResult<Slice> {
    escaped(
        take_while1(|ch: char| ch != '\'' && ch != '"' && ch != '\\'),
        '\\',
        one_of("$\'\\"),
    )(i)
}

fn w<O, G>(parser: G) -> impl FnMut(Slice) -> ParseResult<O>
where
    G: Parser<Slice, O, ParseError>,
{
    preceded(consume_any, parser)
}

fn break_after<O, G>(parser: G) -> impl FnMut(Slice) -> ParseResult<O>
where
    G: Parser<Slice, O, ParseError>,
{
    terminated(parser, consume_any1)
}

fn consume_any(i: Slice) -> ParseResult<()> {
    map(many0(alt((whitespace, line_break, comment))), |_| ())(i)
}

fn consume_any1(i: Slice) -> ParseResult<()> {
    map(many1(alt((whitespace, line_break, comment))), |_| ())(i)
}

fn consume_line(i: Slice) -> ParseResult<()> {
    map(pair(many0(alt((whitespace, comment))), line_break), |_| ())(i)
}

fn comment(i: Slice) -> ParseResult<()> {
    map(
        tuple((tag("#"), take_while(|c| c != '\n' && c != '\r'))),
        |_| (),
    )(i)
}

fn whitespace(i: Slice) -> ParseResult<()> {
    map(take_while1(|c: char| c == ' ' || c == '\t'), |_| ())(i)
}

fn line_break(i: Slice) -> ParseResult<()> {
    map(alt((tag("\r\n"), tag("\n"), tag("\r"))), |_| ())(i)
}
