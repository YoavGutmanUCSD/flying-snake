use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

pub fn init_tracing() {
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new("off"))
        .add_directive("rustyline=off".parse().unwrap());

    tracing_subscriber::registry()
        .with(filter)
        .with(fmt::layer())
        .init();
}
