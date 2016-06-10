# fm-client

网易云音乐客户端。用 Haskell 编写。

参考 [musicbox](https://github.com/darknessomi/musicbox)。

## 安装

```bash
$ sudo apt-get install mpg123 aria2
$ git clone https://github.com/foreverbell/fm-client
$ cd fm-client
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
resolver: lts-5.18
```

```bash
$ stack install
$ fm
```

## 快捷键

<table>
	<tr> <td>Space / Enter</td> <td>确认 / 播放 / 暂停</td> </tr>
	<tr> <td>Esc</td> <td>停止 / 返回上一级菜单</td> </tr>
	<tr> <td>n</td> <td>播放下一首歌曲</td> </tr>
	<tr> <td>o</td> <td>播放模式选择</td> </tr>
	<tr> <td>c</td> <td>缓存选中歌曲</td> </tr>
	<tr> <td>C</td> <td>删除选中歌曲的缓存</td> </tr>
	<tr> <td>-</td> <td>减小音量</td> </tr>
	<tr> <td>=</td> <td>增大音量</td> </tr>
	<tr> <td>m</td> <td>静音 / 取消静音</td> </tr>
</table>
