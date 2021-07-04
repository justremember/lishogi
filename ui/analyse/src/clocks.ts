import { h } from 'snabbdom';
import { VNode } from 'snabbdom/vnode';
import AnalyseCtrl from './ctrl';
import { isFinished } from './study/studyChapters';
import * as game from 'game';

export default function renderClocks(ctrl: AnalyseCtrl): [VNode, VNode] | undefined {
  if (ctrl.embed || ctrl.data.game.id === 'synthetic' || ctrl.data.game.status.name === 'started') return;
  const node = ctrl.node,
    clock = node.clock,
    sentePov = ctrl.bottomIsSente(),
    isSenteTurn = node.ply % 2 === 0;

  if (!clock && clock !== 0)
    return [
      renderOnlyName(ctrl, isSenteTurn, sentePov ? 'bottom' : 'top', 'sente'),
      renderOnlyName(ctrl, !isSenteTurn, sentePov ? 'top' : 'bottom', 'gote'),
    ];

  const parentClock = ctrl.tree.getParentClock(node, ctrl.path),
    centis: Array<number | undefined> = [parentClock, clock];

  if (!isSenteTurn) centis.reverse();

  const study = ctrl.study,
    relay = study && study.data.chapter.relay;
  if (relay && relay.lastMoveAt && relay.path === ctrl.path && ctrl.path !== '' && !isFinished(study!.data.chapter)) {
    const spent = (Date.now() - relay.lastMoveAt) / 10;
    const i = isSenteTurn ? 0 : 1;
    if (centis[i]) centis[i] = Math.max(0, centis[i]! - spent);
  }

  const showTenths = !ctrl.study || !ctrl.study.relay;

  return [
    renderClock(ctrl, centis[0], isSenteTurn, sentePov ? 'bottom' : 'top', 'sente', showTenths),
    renderClock(ctrl, centis[1], !isSenteTurn, sentePov ? 'top' : 'bottom', 'gote', showTenths),
  ];
}

function renderClock(
  ctrl: AnalyseCtrl,
  centis: number | undefined,
  active: boolean,
  cls: string,
  color: Color,
  showTenths: boolean
): VNode {
  return h(
    'div.analyse__clock.' + cls,
    {
      class: { active },
    },
    [spanName(ctrl, color), h('span', {}, clockContent(centis, showTenths))]
  );
}

function clockContent(centis: number | undefined, showTenths: boolean): Array<string | VNode> {
  if (!centis && centis !== 0) return ['-'];
  const date = new Date(centis * 10),
    millis = date.getUTCMilliseconds(),
    sep = ':',
    baseStr = pad2(date.getUTCMinutes()) + sep + pad2(date.getUTCSeconds());
  if (!showTenths || centis >= 360000) return [Math.floor(centis / 360000) + sep + baseStr];
  return centis >= 6000 ? [baseStr] : [baseStr, h('tenths', '.' + Math.floor(millis / 100).toString())];
}

function pad2(num: number): string {
  return (num < 10 ? '0' : '') + num;
}

function spanName(ctrl: AnalyseCtrl, color: Color, sep: string = ' - ') {
  const p = game.getPlayer(ctrl.data, color);
  return h('span', [(p.user ? p.user.username : p.ai ? 'Engine' : 'Anonymous') + sep]);
}

function renderOnlyName(ctrl: AnalyseCtrl, active: boolean, cls: string, color: Color) {
  return h(
    'div.analyse__clock.' + cls,
    {
      class: { active },
    },
    [spanName(ctrl, color, ' ')]
  );
}
