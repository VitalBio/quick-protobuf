use std::path::PathBuf;
use std::str;

use crate::types::{
    Enumerator, Field, FieldType, FileDescriptor, Frequency, Message, OneOf,
    RpcFunctionDeclaration, RpcService, Syntax,
};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric1, digit1, hex_digit1, multispace1, not_line_ending},
    combinator::{map, map_res, opt, recognize, value, verify},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone)]
enum MessageEvent {
    Message(Message),
    Enumerator(Enumerator),
    Field(Field),
    ReservedNums(Vec<i32>),
    ReservedNames(Vec<String>),
    OneOf(OneOf),
    Ignore,
}

#[derive(Debug, Clone)]
enum Event {
    Syntax(Syntax),
    Import(PathBuf),
    Package(String),
    Message(Message),
    Enum(Enumerator),
    RpcService(RpcService),
    Ignore,
}

fn qualifiable_name(input: &str) -> IResult<&str, String> {
    map(
        verify(
            recognize(pair(opt(tag(".")), separated_list1(tag("."), word))),
            |s: &str| !s.ends_with('.') && !s.contains(".."),
        ),
        std::borrow::ToOwned::to_owned,
    )(input)
}

fn word_ref(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((
            // I would really rather just take in 1 alphabetic
            // character, but just using `alpha1()` is also technically
            // correct for our use case and is simpler to implement in
            // nom apparently
            alpha1,
            tag("_"),
        )),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn word(input: &str) -> IResult<&str, String> {
    map(word_ref, |word| word.to_owned())(input)
}

fn hex_integer(input: &str) -> IResult<&str, i32> {
    preceded(
        tag("0x"),
        map_res(hex_digit1, |s: &str| i32::from_str_radix(s, 16)),
    )(input)
}

fn integer(input: &str) -> IResult<&str, i32> {
    map_res(digit1, |s: &str| s.parse())(input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    value((), pair(tag("//"), not_line_ending))(input)
}

fn block_comment(input: &str) -> IResult<&str, ()> {
    value((), tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)
}

fn string(input: &str) -> IResult<&str, String> {
    map(
        delimited(tag("\""), take_until("\""), tag("\"")),
        |s: &str| s.to_owned(),
    )(input)
}

// word break: multispace or comment
fn br(input: &str) -> IResult<&str, ()> {
    value(
        (),
        many1(alt((value((), multispace1), comment, block_comment))),
    )(input)
}

fn syntax(input: &str) -> IResult<&str, Syntax> {
    delimited(
        tuple((tag("syntax"), many0(br), tag("="), many0(br))),
        alt((
            value(Syntax::Proto2, tag("\"proto2\"")),
            value(Syntax::Proto3, tag("\"proto3\"")),
        )),
        pair(many0(br), tag(";")),
    )(input)
}

fn import(input: &str) -> IResult<&str, PathBuf> {
    delimited(
        pair(tag("import"), many1(br)),
        map(string, PathBuf::from),
        pair(many0(br), tag(";")),
    )(input)
}

fn package(input: &str) -> IResult<&str, String> {
    delimited(
        pair(tag("package"), many1(br)),
        qualifiable_name,
        pair(many0(br), tag(";")),
    )(input)
}

fn extensions(input: &str) -> IResult<&str, ()> {
    value((), delimited(tag("extensions"), take_until(";"), tag(";")))(input)
}

fn num_range(input: &str) -> IResult<&str, Vec<i32>> {
    map(
        separated_pair(integer, tuple((many1(br), tag("to"), many1(br))), integer),
        |(from_, to)| (from_..=to).collect(),
    )(input)
}

fn reserved_nums(input: &str) -> IResult<&str, Vec<i32>> {
    map(
        delimited(
            pair(tag("reserved"), many1(br)),
            separated_list1(
                tuple((many0(br), tag(","), many0(br))),
                alt((num_range, map(integer, |i| vec![i]))),
            ),
            pair(many0(br), tag(";")),
        ),
        |nums| nums.into_iter().flat_map(|v| v.into_iter()).collect(),
    )(input)
}

fn reserved_names(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        pair(tag("reserved"), many1(br)),
        separated_list1(tuple((many0(br), tag(","), many0(br))), string),
        pair(many0(br), tag(";")),
    )(input)
}

fn option_key(input: &str) -> IResult<&str, String> {
    // Helper parser for dot-separated words
    fn dot_separated_words(input: &str) -> IResult<&str, Vec<&str>> {
        separated_list1(tag("."), word_ref)(input)
    }

    // Parser for the extension form: (foo.bar).baz.qux or just (foo.bar)
    let extension = map(
        tuple((
            delimited(tag("("), dot_separated_words, tag(")")),
            opt(pair(tag("."), dot_separated_words)),
        )),
        |(ext_words, opt_field)| {
            match opt_field {
                Some((_, field_words)) => format!("({}).{}", ext_words.join("."), field_words.join(".")),
                None => format!("({})", ext_words.join(".")),
            }
        },
    );

    // Parser for the simple word form
    let simple = map(word_ref, |w| w.to_string());

    alt((extension, simple))(input)
}

fn key_val(input: &str) -> IResult<&str, (String, &str)> {
    let (input, _) = many0(br)(input)?;
    let (input, key) = option_key(input)?;
    let (input, _) = many0(br)(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = many0(br)(input)?;
    // Take until either ']' or ',' unless inside a string
    let mut chars = input.char_indices().peekable();
    let mut in_string = false;
    let mut end_pos = None;

    while let Some((i, c)) = chars.next() {
        match c {
            '"' => {
                in_string = !in_string;
            }
            ']' | ',' if !in_string => {
                end_pos = Some(i);
                break;
            }
            _ => {}
        }
    }

    let value_end = end_pos.unwrap_or(input.len());
    let value = &input[..value_end];
    let input = &input[value_end..];
    let (input, _) = many0(br)(input)?;
    Ok((input, (key, value.trim())))
}

fn key_vals(input: &str) -> IResult<&str, Vec<(String, &str)>> {
    opt(delimited(
        tag("["),
        separated_list1(tag(","), key_val),
        tag("]"),
    ))(input)
    .map(|(next_input, opt_vec)| (next_input, opt_vec.unwrap_or_default()))
}

fn frequency(input: &str) -> IResult<&str, Frequency> {
    alt((
        value(Frequency::Optional, tag("optional")),
        value(Frequency::Repeated, tag("repeated")),
        value(Frequency::Required, tag("required")),
    ))(input)
}

fn field_type(input: &str) -> IResult<&str, FieldType> {
    alt((
        value(FieldType::Int32, tag("int32")),
        value(FieldType::Int64, tag("int64")),
        value(FieldType::Uint32, tag("uint32")),
        value(FieldType::Uint64, tag("uint64")),
        value(FieldType::Sint32, tag("sint32")),
        value(FieldType::Sint64, tag("sint64")),
        value(FieldType::Fixed32, tag("fixed32")),
        value(FieldType::Sfixed32, tag("sfixed32")),
        value(FieldType::Fixed64, tag("fixed64")),
        value(FieldType::Sfixed64, tag("sfixed64")),
        value(FieldType::Bool, tag("bool")),
        value(FieldType::String_, tag("string")),
        value(FieldType::Bytes_, tag("bytes")),
        value(FieldType::Float, tag("float")),
        value(FieldType::Double, tag("double")),
        map(qualifiable_name, |w| FieldType::MessageOrEnum(w)),
    ))(input)
}

fn message_field(input: &str) -> IResult<&str, Field> {
    map(
        tuple((
            opt(terminated(frequency, many1(br))),
            terminated(field_type, many1(br)),
            separated_pair(
                word,
                delimited(many0(br), tag("="), many0(br)),
                alt((integer, hex_integer)),
            ),
            delimited(many0(br), key_vals, pair(many0(br), tag(";"))),
        )),
        |(freq, typ, (name, number), key_vals)| Field {
            name,
            frequency: freq.unwrap_or(Frequency::Optional),
            number,
            default: key_vals.iter().find_map(|&(ref k, v)| {
                if k == "default" {
                    Some(v.to_string())
                } else {
                    None
                }
            }),
            packed: key_vals.iter().find_map(|&(ref k, v)| {
                if k == "packed" {
                    Some(v.parse().expect("Cannot parse Packed value"))
                } else {
                    None
                }
            }),
            typ,
            deprecated: key_vals
                .iter()
                .find_map(|&(ref k, v)| {
                    if k == "deprecated" {
                        Some(v.parse().expect("Cannot parse Deprecated value"))
                    } else {
                        None
                    }
                })
                .unwrap_or(false),
            max_length: key_vals.iter().find_map(|&(ref k, v)| {
                if k == "(vital_options.rust).max_length" {
                    Some(
                        v.parse::<u32>()
                            .expect("Cannot parse (vital_options.rust).max_length value"),
                    )
                } else {
                    None
                }
            }),
            cfg: key_vals.iter().find_map(|&(ref k, v)| {
                if k == "(vital_options.rust).cfg" {
                    Some(
                        transform_cfg_value(v)
                            .expect("Cannot parse (vital_options.rust).cfg value"),
                    )
                } else {
                    None
                }
            }),
        },
    )(input)
}

fn transform_cfg_value(s: &str) -> Result<String, &'static str> {
    if s.len() < 2 {
        return Err("cfg value too short");
    }

    if s.chars().nth(0) != Some('"') {
        return Err("cfg value doesn't start with \"");
    }
    if s.chars().last() != Some('"') {
        return Err("cfg value doesn't end with \"");
    }

    let s = s.replace("<COMMA>", ",");

    Ok(s[1..s.len() - 1].replace("\\\"", "\""))
}

fn one_of(input: &str) -> IResult<&str, OneOf> {
    map(
        pair(
            preceded(pair(tag("oneof"), many1(br)), word),
            delimited(
                pair(many0(br), tag("{")),
                many1(delimited(many0(br), message_field, many0(br))),
                tag("}"),
            ),
        ),
        |(name, fields)| OneOf {
            name,
            fields,
            package: "".to_string(),
            module: "".to_string(),
            imported: false,
        },
    )(input)
}

fn rpc_function_declaration(input: &str) -> IResult<&str, RpcFunctionDeclaration> {
    map(
        tuple((
            delimited(pair(tag("rpc"), many1(br)), word, many0(br)),
            delimited(pair(tag("("), many0(br)), word, pair(many0(br), tag(")"))),
            delimited(
                tuple((many1(br), tag("returns"), many0(br), tag("("), many0(br))),
                word,
                pair(many0(br), tag(")")),
            ),
            preceded(
                many0(br),
                alt((
                    value(
                        (),
                        delimited(
                            pair(tag("{"), many0(br)),
                            many0(alt((option_ignore, value((), tag(";"))))),
                            pair(many0(br), tag("}")),
                        ),
                    ),
                    value((), tag(";")),
                )),
            ),
        )),
        |(name, arg, ret, _)| RpcFunctionDeclaration { name, arg, ret },
    )(input)
}

fn rpc_service(input: &str) -> IResult<&str, RpcService> {
    map(
        pair(
            delimited(pair(tag("service"), many1(br)), word, many0(br)),
            delimited(
                tag("{"),
                many0(delimited(many0(br), rpc_function_declaration, many0(br))),
                tag("}"),
            ),
        ),
        |(service_name, functions)| RpcService {
            service_name,
            functions,
        },
    )(input)
}

fn message_event(input: &str) -> IResult<&str, MessageEvent> {
    alt((
        map(reserved_nums, |r| MessageEvent::ReservedNums(r)),
        map(reserved_names, |r| MessageEvent::ReservedNames(r)),
        map(message_field, |f| MessageEvent::Field(f)),
        map(message, |m| MessageEvent::Message(m)),
        map(enumerator, |e| MessageEvent::Enumerator(e)),
        map(one_of, |o| MessageEvent::OneOf(o)),
        value(MessageEvent::Ignore, extensions),
        value(MessageEvent::Ignore, br),
    ))(input)
}

fn message(input: &str) -> IResult<&str, Message> {
    map(
        terminated(
            pair(
                delimited(pair(tag("message"), many1(br)), word, many0(br)),
                delimited(tag("{"), many0(message_event), tag("}")),
            ),
            opt(pair(many0(br), tag(";"))),
        ),
        |(name, events)| {
            let mut msg = Message {
                name,
                ..Default::default()
            };
            for e in events {
                match e {
                    MessageEvent::Field(f) => msg.fields.push(f),
                    MessageEvent::ReservedNums(r) => msg.reserved_nums = Some(r),
                    MessageEvent::ReservedNames(r) => msg.reserved_names = Some(r),
                    MessageEvent::Message(m) => msg.messages.push(m),
                    MessageEvent::Enumerator(e) => msg.enums.push(e),
                    MessageEvent::OneOf(o) => msg.oneofs.push(o),
                    MessageEvent::Ignore => (),
                }
            }
            msg
        },
    )(input)
}

fn enum_field(input: &str) -> IResult<&str, (String, i32)> {
    terminated(
        separated_pair(
            word,
            tuple((many0(br), tag("="), many0(br))),
            alt((hex_integer, integer)),
        ),
        pair(many0(br), tag(";")),
    )(input)
}

fn enumerator(input: &str) -> IResult<&str, Enumerator> {
    map(
        pair(
            delimited(pair(tag("enum"), many1(br)), word, many0(br)),
            delimited(
                pair(tag("{"), many0(br)),
                separated_list0(br, enum_field),
                pair(many0(br), tag("}")),
            ),
        ),
        |(name, fields)| Enumerator {
            name,
            fields,
            ..Default::default()
        },
    )(input)
}

fn option_ignore(input: &str) -> IResult<&str, ()> {
    value(
        (),
        tuple((
            tag("option"),
            many1(br),
            take_until("="),
            tag("="),
            many1(br),
            many0(string),
            take_until(";"),
            tag(";"),
        )),
    )(input)
}

pub fn file_descriptor(input: &str) -> IResult<&str, FileDescriptor, nom::error::Error<String>> {
    let mut parser = map(
        many0(alt((
            map(syntax, |s| Event::Syntax(s)),
            map(import, |i| Event::Import(i)),
            map(package, |p| Event::Package(p)),
            map(message, |m| Event::Message(m)),
            map(enumerator, |e| Event::Enum(e)),
            map(rpc_service, |r| Event::RpcService(r)),
            value(Event::Ignore, option_ignore),
            value(Event::Ignore, br),
        ))),
        |events| {
            let mut desc = FileDescriptor::default();
            println!("    Events: {:?}", events);
            for event in events {
                match event {
                    Event::Syntax(s) => desc.syntax = s,
                    Event::Import(i) => desc.import_paths.push(i),
                    Event::Package(p) => desc.package = p,
                    Event::Message(m) => desc.messages.push(m),
                    Event::Enum(e) => desc.enums.push(e),
                    Event::RpcService(r) => desc.rpc_services.push(r),
                    Event::Ignore => (),
                }
            }
            desc
        },
    );

    let (unparsed, desc) = parser(input).map_err(|e: nom::Err<nom::error::Error<&str>>| e.to_owned())?;

    if !unparsed.is_empty() {
        eprintln!("Parsing failed at:\n{}...\nIs there a syntax error?", unparsed.chars().take(50).collect::<String>());
        let e: nom::error::Error<String> = nom::error::make_error(unparsed.to_owned(), nom::error::ErrorKind::Eof);
        return Err(nom::Err::Failure(e));
    }
    Ok(("", desc))
}

#[cfg(test)]
mod test {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_message() {
        let msg = r#"message ReferenceData
    {
        repeated ScenarioInfo  scenarioSet = 1;
        repeated CalculatedObjectInfo calculatedObjectSet = 2;
        repeated RiskFactorList riskFactorListSet = 3;
        repeated RiskMaturityInfo riskMaturitySet = 4;
        repeated IndicatorInfo indicatorSet = 5;
        repeated RiskStrikeInfo riskStrikeSet = 6;
        repeated FreeProjectionList freeProjectionListSet = 7;
        repeated ValidationProperty ValidationSet = 8;
        repeated CalcProperties calcPropertiesSet = 9;
        repeated MaturityInfo maturitySet = 10;
    }"#;

        let mess = message(msg);
        if let ::nom::IResult::Ok((_, mess)) = mess {
            assert_eq!(10, mess.fields.len());
        }
    }

    #[test]
    fn test_enum() {
        let msg = r#"enum PairingStatus {
                DEALPAIRED        = 0;
                INVENTORYORPHAN   = 1;
                CALCULATEDORPHAN  = 2;
                CANCELED          = 3;
    }"#;

        let mess = enumerator(msg);
        if let ::nom::IResult::Ok((_, mess)) = mess {
            assert_eq!(4, mess.fields.len());
        }
    }

    #[test]
    fn test_ignore() {
        let msg = r#"option optimize_for = SPEED;"#;

        match option_ignore(msg) {
            ::nom::IResult::Ok(_) => (),
            e => panic!("Expecting done {:?}", e),
        }
    }

    #[test]
    fn test_comments() {
        assert_eq!("\nb", comment("// BOOM\nb").unwrap().0);
        assert_eq!("\nb", comment("//\nb").unwrap().0);
        assert_eq!("\nb", block_comment("/* BOOM */\nb").unwrap().0);
        let msg = r#"
            // BOOM
            /* BOOM */
            package foo.bar;
            // BOOM
            /* BOOM */
            message A {
                // BOOM
                enum E1 {
                    // BOOM
                    // BOOM
                    V1 = 1;
                    // BOOM
                    v2 = 2;
                }
                // BOOM
                enum E2 {
                    // BOOM
                }
                enum E3 { /* BOOM */ }
                // BOOM
                message B {
                    // BOOM
                    // BOOM
                    optional string b = 1;
                }
                message C {
                    // BOOM
                }
                message D { /* BOOM */ }
                required string a = 1;
            }
            "#;
        let desc = file_descriptor(msg).unwrap().1;
        assert_eq!("foo.bar".to_string(), desc.package);
        assert_eq!(1, desc.messages.len());
        assert_eq!(3, desc.messages[0].messages.len());
        assert_eq!(3, desc.messages[0].enums.len());
    }

    #[test]
    fn test_import() {
        let msg = r#"syntax = "proto3";

    import "test_import_nested_imported_pb.proto";

    message ContainsImportedNested {
        optional ContainerForNested.NestedMessage m = 1;
        optional ContainerForNested.NestedEnum e = 2;
    }
    "#;
        let desc = file_descriptor(msg).unwrap().1;
        assert_eq!(
            vec![Path::new("test_import_nested_imported_pb.proto")],
            desc.import_paths
        );
    }

    #[test]
    fn test_package() {
        let msg = r#"
        package foo.bar;

    message ContainsImportedNested {
        optional ContainerForNested.NestedMessage m = 1;
        optional ContainerForNested.NestedEnum e = 2;
    }
    "#;
        let desc = file_descriptor(msg).unwrap().1;
        assert_eq!("foo.bar".to_string(), desc.package);
    }

    #[test]
    fn test_nested_message() {
        let msg = r#"message A
    {
        message B {
            repeated int32 a = 1;
            optional string b = 2;
        }
        message C {
        }
        message D {}
        optional int32 b = 1;
    }"#;

        let desc = file_descriptor(msg).unwrap().1;
        dbg!(&desc);
        assert_eq!(1, desc.messages.len());
        assert_eq!(3, desc.messages[0].messages.len());
    }

    #[test]
    fn test_oneof() {
        let msg = r#"message A
    {
        optional int32 a1 = 1;
        oneof a_oneof {
            string a2 = 2;
            int32 a3 = 3;
            bytes a4 = 4;
        }
        repeated bool a5 = 5;
    }"#;

        let mess = message(msg);
        if let ::nom::IResult::Ok((_, mess)) = mess {
            assert_eq!(1, mess.oneofs.len());
            assert_eq!(3, mess.oneofs[0].fields.len());
        }
    }

    #[test]
    fn test_reserved() {
        let msg = r#"message Sample {
       reserved 4, 15, 17 to 20, 30;
       reserved "foo", "bar";
       uint64 age =1;
       bytes name =2;
    }"#;

        let mess = message(msg);
        if let ::nom::IResult::Ok((_, mess)) = mess {
            assert_eq!(Some(vec![4, 15, 17, 18, 19, 20, 30]), mess.reserved_nums);
            assert_eq!(
                Some(vec!["foo".to_string(), "bar".to_string()]),
                mess.reserved_names
            );
            assert_eq!(2, mess.fields.len());
        } else {
            panic!("Could not parse reserved fields message");
        }
    }

    #[test]
    fn test_rpc_service() {
        let msg = r#"
            service RpcService {
                rpc function0(InStruct0) returns (OutStruct0);
                rpc function1(InStruct1) returns (OutStruct1);
                rpc function2  (  InStruct2  ) returns (  OutStruct2  ) {  }
            }
        "#;

        match file_descriptor(msg) {
            ::nom::IResult::Ok((_, descriptor)) => {
                println!("Services found: {:?}", descriptor.rpc_services);
                let service = &descriptor.rpc_services.get(0).expect("Service not found!");
                let func0 = service.functions.get(0).expect("Function 0 not returned!");
                let func1 = service.functions.get(1).expect("Function 1 not returned!");
                let func2 = service.functions.get(2).expect("Function 2 not returned!");
                assert_eq!("RpcService", service.service_name);
                assert_eq!("function0", func0.name);
                assert_eq!("InStruct0", func0.arg);
                assert_eq!("OutStruct0", func0.ret);
                assert_eq!("function1", func1.name);
                assert_eq!("InStruct1", func1.arg);
                assert_eq!("OutStruct1", func1.ret);
                assert_eq!("function2", func2.name);
                assert_eq!("InStruct2", func2.arg);
                assert_eq!("OutStruct2", func2.ret);
            }
            other => panic!("Could not parse RPC Service: {:?}", other),
        }
    }

    #[test]
    fn test_rpc_function() {
        let msg = r#"rpc function_name(Arg) returns (Ret);"#;

        match rpc_function_declaration(msg) {
            ::nom::IResult::Ok((_, declaration)) => {
                assert_eq!("function_name", declaration.name);
                assert_eq!("Arg", declaration.arg);
                assert_eq!("Ret", declaration.ret);
            }
            other => panic!("Could not parse RPC Function Declaration: {:?}", other),
        }
    }

    #[test]
    fn test_option_key_plain() {
        let msg = r#"test"#;

        match option_key(msg) {
            ::nom::IResult::Ok((_, s)) => assert_eq!("test", s),
            other => panic!("Expecting done {:?}", other),
        }
    }

    #[test]
    fn test_option_key_parens() {
        let msg = r#"(test)"#;

        match option_key(msg) {
            // ::nom::IResult::Ok((_, s)) => assert_eq!("test", s),
            ::nom::IResult::Ok((_, s)) => {
                println!("string = {s}");
                assert_eq!("(test)", s)
            }
            other => panic!("Expecting done {:?}", other),
        }
    }
}
