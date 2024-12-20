use wasmtime::{Instance, Module, Store};

pub fn run(wat: &str) -> Result<i32, wasmtime::Error> {
    let mut store = Store::<()>::default();
    let module = Module::new(store.engine(), wat)?;
    let instance = Instance::new(&mut store, &module, &[])?;

    // Invoke `gcd` export
    let main = instance.get_typed_func::<(), i32>(&mut store, "main")?;

    let res = main.call(&mut store, ())?;
    Ok(res)
}
