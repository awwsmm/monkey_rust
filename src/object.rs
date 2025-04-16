type ObjectType = String;

trait Object {
    fn object_type() -> ObjectType;
    fn inspect() -> String;
}