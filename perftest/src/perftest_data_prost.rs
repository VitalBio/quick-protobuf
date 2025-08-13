#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Test1 {
    #[prost(int32, optional, tag="1")]
    pub value: ::core::option::Option<i32>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct TestRepeatedBool {
    #[prost(bool, repeated, packed="false", tag="1")]
    pub values: ::prost::alloc::vec::Vec<bool>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct TestRepeatedPackedInt32 {
    #[prost(int32, repeated, tag="1")]
    pub values: ::prost::alloc::vec::Vec<i32>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct TestRepeatedMessages {
    #[prost(message, repeated, tag="1")]
    pub messages1: ::prost::alloc::vec::Vec<TestRepeatedMessages>,
    #[prost(message, repeated, tag="2")]
    pub messages2: ::prost::alloc::vec::Vec<TestRepeatedMessages>,
    #[prost(message, repeated, tag="3")]
    pub messages3: ::prost::alloc::vec::Vec<TestRepeatedMessages>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct TestOptionalMessages {
    #[prost(message, optional, boxed, tag="1")]
    pub message1: ::core::option::Option<::prost::alloc::boxed::Box<TestOptionalMessages>>,
    #[prost(message, optional, boxed, tag="2")]
    pub message2: ::core::option::Option<::prost::alloc::boxed::Box<TestOptionalMessages>>,
    #[prost(message, optional, boxed, tag="3")]
    pub message3: ::core::option::Option<::prost::alloc::boxed::Box<TestOptionalMessages>>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct TestStrings {
    #[prost(string, optional, tag="1")]
    pub s1: ::core::option::Option<::prost::alloc::string::String>,
    #[prost(string, optional, tag="2")]
    pub s2: ::core::option::Option<::prost::alloc::string::String>,
    #[prost(string, optional, tag="3")]
    pub s3: ::core::option::Option<::prost::alloc::string::String>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct TestBytes {
    #[prost(bytes="vec", optional, tag="1")]
    pub b1: ::core::option::Option<::prost::alloc::vec::Vec<u8>>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct PerftestData {
    #[prost(message, repeated, tag="1")]
    pub test1: ::prost::alloc::vec::Vec<Test1>,
    #[prost(message, repeated, tag="2")]
    pub test_repeated_bool: ::prost::alloc::vec::Vec<TestRepeatedBool>,
    #[prost(message, repeated, tag="3")]
    pub test_repeated_messages: ::prost::alloc::vec::Vec<TestRepeatedMessages>,
    #[prost(message, repeated, tag="4")]
    pub test_optional_messages: ::prost::alloc::vec::Vec<TestOptionalMessages>,
    #[prost(message, repeated, tag="5")]
    pub test_strings: ::prost::alloc::vec::Vec<TestStrings>,
    #[prost(message, repeated, tag="6")]
    pub test_repeated_packed_int32: ::prost::alloc::vec::Vec<TestRepeatedPackedInt32>,
    #[prost(message, repeated, tag="7")]
    pub test_small_bytearrays: ::prost::alloc::vec::Vec<TestBytes>,
    #[prost(message, repeated, tag="8")]
    pub test_large_bytearrays: ::prost::alloc::vec::Vec<TestBytes>,
}
