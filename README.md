# Typing Race Server

Servidor y cliente web simple para una carrera de tipeo en tiempo real usando Scala 3, Apache Pekko HTTP y WebSocket.

## Requisitos

- JDK 17 o superior
- sbt 1.10.7 o superior

## Ejecutar

```bash
sbt run
```

Luego abrir:

```text
http://localhost:8080/
```

## Probar

```bash
sbt test
```

## Estructura

- `src/main/scala/com/typerace/domain`: modelo y logica del juego
- `src/main/scala/com/typerace/actor`: actores que coordinan sesiones y estado
- `src/main/scala/com/typerace/http`: servidor HTTP y rutas WebSocket
- `src/main/resources/web/index.html`: cliente web
