use bevy::input::common_conditions::input_pressed;
use bevy::input::mouse::{MouseMotion, MouseWheel};
use bevy::log::LogPlugin;
use bevy::prelude::*;
use bevy::sprite::MaterialMesh2dBundle;
use bevy::winit::WinitSettings;
use color_eyre::eyre::Result;

#[derive(Component)]
struct Camera;

#[derive(Component)]
struct Ui;

/// https://bevyengine.org/learn/quick-start/getting-started/ecs/
const ALIGN_ITEMS_COLOR: Color = Color::rgb(1., 0.066, 0.349);
const JUSTIFY_CONTENT_COLOR: Color = Color::rgb(0.102, 0.522, 1.);
const MARGIN: Val = Val::Px(5.);

//https://bevy-cheatbook.github.io/input/mouse.html
// https://bevy-cheatbook.github.io/features/camera.html
fn zoom_camera(
  mut camera_project_query: Query<&mut OrthographicProjection, With<Camera>>,
  mut scroll_events: EventReader<MouseWheel>,
) {
  use bevy::input::mouse::MouseScrollUnit;
  for ev in scroll_events.read() {
    // info!("Got an event {ev:?}");
    match ev.unit {
      MouseScrollUnit::Line => {
        // println!("Scroll (line units): vertical: {}, horizontal: {}", ev.y, ev.x);
      }
      MouseScrollUnit::Pixel => {
        let mut projection = camera_project_query.single_mut();

        projection.scale *= 1.0 - (ev.y / 1000.);

        // always ensure you end up with sane values
        // (pick an upper and lower bound for your application)
        projection.scale = projection.scale.clamp(0.2, 100.0);
      }
    }
  }
}

fn move_camera(
  projection_query: Query<&mut OrthographicProjection, With<Camera>>,
  mut camera_position_query: Query<&mut Transform, With<Camera>>,
  mut motion_evr: EventReader<MouseMotion>,
) {
  let projection = projection_query.single();

  let mut camera_position = camera_position_query.single_mut();
  for move_event in motion_evr.read() {
    let scale = projection.scale.clamp(1.0, 5.0);
    camera_position.translation.x -= move_event.delta.x * scale;
    camera_position.translation.y += move_event.delta.y * scale;
  }
}

/// https://github.com/bevyengine/bevy/blob/latest/examples/ui/flex_layout.rs
/// <https://taintedcoders.com/bevy/ui/>
fn main() -> Result<()> {
  color_eyre::install()?;
  App::new()
    .insert_resource(WinitSettings::desktop_app())
    .add_plugins(
      DefaultPlugins
        .set(WindowPlugin {
          primary_window: Some(Window {
            resolution: [870., 1066.].into(),
            title: "ICFP 2024 - Contest Visualizer".to_string(),
            ..Default::default()
          }),
          ..Default::default()
        })
        .set(LogPlugin {
          filter: "info,wgpu_core=warn,wgpu_hal=warn".into(),
          level: bevy::log::Level::DEBUG,
          update_subscriber: None,
        }),
    )
    .add_systems(Startup, spawn_layout)
    .add_systems(
      Update,
      (
        zoom_camera,
        move_camera.run_if(input_pressed(MouseButton::Left)),
        keyboard_input_system,
      ),
    )
    .run();

  Ok(())
}

/// This system prints 'A' key state
fn keyboard_input_system(
  keyboard_input: Res<ButtonInput<KeyCode>>,
  mut main_node: Query<&mut Visibility, With<Ui>>,
) {
  let mut viz = main_node.single_mut();
  if keyboard_input.pressed(KeyCode::Escape) {
    *viz = Visibility::Visible;
  }

  if keyboard_input.just_released(KeyCode::Escape) {
    *viz = Visibility::Hidden;
  }
}

fn spawn_layout(
  mut commands: Commands,
  asset_server: Res<AssetServer>,
  mut meshes: ResMut<Assets<Mesh>>,
  mut materials: ResMut<Assets<ColorMaterial>>,
) {
  let font = asset_server.load("fonts/Hack-Bold.ttf");
  // commands.spawn(Camera2dBundle::default());
  commands.insert_resource(ClearColor(Color::CYAN));

  commands.spawn((Camera2dBundle::new_with_far(100.), Camera));

  commands.spawn(MaterialMesh2dBundle {
    mesh: meshes.add(Rectangle::new(600.0, 800.0)).into(),
    material: materials.add(ColorMaterial::from(Color::BLACK)),
    transform: Transform::from_xyz(0.0, 0.0, 0.0),
    ..default()
  });

  commands
    .spawn((
      NodeBundle {
        style: Style {
          // fill the entire window
          width: Val::Percent(100.),
          flex_direction: FlexDirection::Column,
          align_items: AlignItems::Center,
          ..Default::default()
        },
        visibility: Visibility::Hidden,
        background_color: BackgroundColor(Color::BLACK),
        ..Default::default()
      },
      Ui,
    ))
    .with_children(|builder| {
      // spawn the key
      builder
        .spawn(NodeBundle {
          style: Style {
            flex_direction: FlexDirection::Row,
            margin: UiRect::top(MARGIN),
            ..Default::default()
          },
          ..Default::default()
        })
        .with_children(|builder| {
          spawn_nested_text_bundle(
            builder,
            font.clone(),
            ALIGN_ITEMS_COLOR,
            UiRect::right(MARGIN),
            "AlignItems",
          );
          spawn_nested_text_bundle(
            builder,
            font.clone(),
            JUSTIFY_CONTENT_COLOR,
            UiRect::default(),
            "JustifyContent",
          );
        });

      builder
        .spawn(NodeBundle {
          style: Style {
            width: Val::Px(850.),
            height: Val::Px(1020.),
            flex_direction: FlexDirection::Column,
            ..Default::default()
          },
          ..Default::default()
        })
        .with_children(|builder| {
          // spawn one child node for each combination of `AlignItems` and `JustifyContent`
          let justifications = [
            JustifyContent::FlexStart,
            JustifyContent::Center,
            JustifyContent::FlexEnd,
            JustifyContent::SpaceEvenly,
            JustifyContent::SpaceAround,
            JustifyContent::SpaceBetween,
          ];
          let alignments = [
            AlignItems::Baseline,
            AlignItems::FlexStart,
            AlignItems::Center,
            AlignItems::FlexEnd,
            AlignItems::Stretch,
          ];
          for justify_content in justifications {
            builder
              .spawn(NodeBundle {
                style: Style {
                  flex_direction: FlexDirection::Row,
                  ..Default::default()
                },
                ..Default::default()
              })
              .with_children(|builder| {
                for align_items in alignments {
                  spawn_child_node(builder, font.clone(), align_items, justify_content);
                }
              });
          }
        });
    });
}

fn spawn_child_node(
  builder: &mut ChildBuilder,
  font: Handle<Font>,
  align_items: AlignItems,
  justify_content: JustifyContent,
) {
  builder
    .spawn(NodeBundle {
      style: Style {
        flex_direction: FlexDirection::Column,
        align_items,
        justify_content,
        width: Val::Px(160.),
        height: Val::Px(160.),
        margin: UiRect::all(MARGIN),
        ..Default::default()
      },
      background_color: BackgroundColor(Color::DARK_GRAY),
      ..Default::default()
    })
    .with_children(|builder| {
      let labels = [
        (format!("{align_items:?}"), ALIGN_ITEMS_COLOR, 0.),
        (format!("{justify_content:?}"), JUSTIFY_CONTENT_COLOR, 3.),
      ];
      for (text, color, top_margin) in labels {
        // We nest the text within a parent node because margins and padding can't be directly applied to text nodes currently.
        spawn_nested_text_bundle(
          builder,
          font.clone(),
          color,
          UiRect::top(Val::Px(top_margin)),
          &text,
        );
      }
    });
}

fn spawn_nested_text_bundle(
  builder: &mut ChildBuilder,
  font: Handle<Font>,
  background_color: Color,
  margin: UiRect,
  text: &str,
) {
  builder
    .spawn(NodeBundle {
      style: Style {
        margin,
        padding: UiRect::axes(Val::Px(5.), Val::Px(1.)),
        ..Default::default()
      },
      background_color: BackgroundColor(background_color),
      ..Default::default()
    })
    .with_children(|builder| {
      builder.spawn(TextBundle::from_section(
        text,
        TextStyle {
          font,
          font_size: 24.0,
          color: Color::BLACK,
        },
      ));
    });
}
