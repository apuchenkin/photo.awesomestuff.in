import { Match as BaseMatch } from "found";

export interface MatchContext {
  services: any,
  config: Config,
}

export interface Match extends BaseMatch {
  context: MatchContext;
}