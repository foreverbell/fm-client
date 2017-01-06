# netease-fm

网易云音乐客户端。用 Haskell 编写。

参考 [musicbox](https://github.com/darknessomi/musicbox)。

## 安装

```bash
$ sudo apt-get install mpg123 aria2
$ git clone https://github.com/foreverbell/netease-fm
$ cd netease-fm
$ cabal install
$ fm
```

建议使用 stack，`stack.yaml` 添加 `extra-deps` 项使用 `brick-0.6.4`。

```yaml
flags: {}
extra-package-dbs: []
extra-deps: 
- brick-0.6.4
- vty-5.5.0
resolver: lts-6.27
```

```bash
$ stack install
$ fm
```

## 快捷键

| 快捷键        | 功能                  |
| ------------- | --------------------- |
| Space / Enter | 确认 / 播放 / 暂停    |
| Esc           | 停止 / 返回上一级菜单 |
| n             | 播放下一首歌曲        |
| o             | 播放模式选择          |
| c             | 缓存选中歌曲          |
| C             | 删除选中歌曲的缓存    |
| -             | 减小音量              |
| =             | 增大音量              |
| m             | 静音 / 取消静音       |
